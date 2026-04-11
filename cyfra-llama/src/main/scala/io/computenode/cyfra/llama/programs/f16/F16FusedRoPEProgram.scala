package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Fused F16 Rotary Position Embedding for both Q and K in single dispatch.
  *
  * Reduces dispatch count by processing both Q and K tensors simultaneously.
  * Each invocation handles one pair from either Q or K.
  */
object F16FusedRoPEProgram:
  val BLOCK_SIZE = 256

  case class Sizes(
    B: Int,
    T: Int,
    numHeadsQ: Int,
    numHeadsK: Int,
    headSize: Int,
    theta: Float,
  ):
    def totalQPairs: Int = B * T * numHeadsQ * (headSize / 2)
    def totalKPairs: Int = B * T * numHeadsK * (headSize / 2)
    def totalPairs: Int = totalQPairs + totalKPairs
  
  case class ProgramLayout(
    qIn: GBuffer[Float16],
    kIn: GBuffer[Float16],
    qOut: GBuffer[Float16],
    kOut: GBuffer[Float16],
    params: GUniform[AttentionParams],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val numHeadsQ = sizes.numHeadsQ
    val numHeadsK = sizes.numHeadsK
    val headSize = sizes.headSize
    val theta = sizes.theta
    val totalQPairs = sizes.totalQPairs
    val totalKPairs = sizes.totalKPairs
    val totalPairs = sizes.totalPairs

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        qIn = GBuffer[Float16](s.B * s.T * s.numHeadsQ * s.headSize),
        kIn = GBuffer[Float16](s.B * s.T * s.numHeadsK * s.headSize),
        qOut = GBuffer[Float16](s.B * s.T * s.numHeadsQ * s.headSize),
        kOut = GBuffer[Float16](s.B * s.T * s.numHeadsK * s.headSize),
        params = GUniform[AttentionParams](),
      ),
      dispatch = (_, s) => StaticDispatch(((s.totalPairs + BLOCK_SIZE - 1) / BLOCK_SIZE, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val totalPairsVal: Int32 = totalPairs
      val totalQPairsVal: Int32 = totalQPairs
      val Tval: Int32 = T
      val numHeadsQVal: Int32 = numHeadsQ
      val numHeadsKVal: Int32 = numHeadsK
      val halfHead: Int32 = headSize / 2
      val headSizeVal: Int32 = headSize
      val thetaVal: Float32 = theta
      val startPosVal: Int32 = layout.params.read.startPos

      GIO.when(idx < totalPairsVal):
        // Determine if this is Q or K based on index
        val isQ = idx < totalQPairsVal
        
        // Calculate local index within Q or K
        val localIdx: Int32 = when(isQ)(idx).otherwise(idx - totalQPairsVal)
        val numHeads: Int32 = when(isQ)(numHeadsQVal).otherwise(numHeadsKVal)
        
        // Decompose index
        val perHead = halfHead
        val perPos = numHeads * halfHead
        val perBatch = Tval * perPos

        val b = localIdx / perBatch
        val rem1 = localIdx.mod(perBatch)
        val t = rem1 / perPos
        val rem2 = rem1.mod(perPos)
        val h = rem2 / perHead
        val d = rem2.mod(perHead)

        // Compute RoPE rotation
        val pos = startPosVal + t
        val headSizeFloat: Float32 = headSize.toFloat
        val freqExponent: Float32 = -2.0f * d.asFloat / headSizeFloat
        val freq: Float32 = pos.asFloat * pow(thetaVal, freqExponent)
        val cosFreq = cos(freq).asFloat16
        val sinFreq = sin(freq).asFloat16

        // Calculate full indices for reading/writing
        val fullIdx: Int32 = b * Tval * numHeads * headSizeVal + t * numHeads * headSizeVal + h * headSizeVal
        val idx0 = fullIdx + d * 2
        val idx1 = idx0 + 1

        // Read, rotate, write - branch on Q vs K
        for
          _ <- GIO.when(isQ):
            val x0 = GIO.read[Float16](layout.qIn, idx0)
            val x1 = GIO.read[Float16](layout.qIn, idx1)
            val y0 = x0 * cosFreq - x1 * sinFreq
            val y1 = x0 * sinFreq + x1 * cosFreq
            for
              _ <- GIO.write[Float16](layout.qOut, idx0, y0)
              _ <- GIO.write[Float16](layout.qOut, idx1, y1)
            yield GStruct.Empty()
          _ <- GIO.when(!isQ):
            val x0 = GIO.read[Float16](layout.kIn, idx0)
            val x1 = GIO.read[Float16](layout.kIn, idx1)
            val y0 = x0 * cosFreq - x1 * sinFreq
            val y1 = x0 * sinFreq + x1 * cosFreq
            for
              _ <- GIO.write[Float16](layout.kOut, idx0, y0)
              _ <- GIO.write[Float16](layout.kOut, idx1, y1)
            yield GStruct.Empty()
        yield GStruct.Empty()
