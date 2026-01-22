package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.llama.programs.AttentionParams

/** Rotary Position Embeddings (RoPE).
  * 
  * RoPE applies a rotation to pairs of elements based on their position:
  *   x_rot[2i]   = x[2i] * cos(pos * theta^(-2i/d)) - x[2i+1] * sin(pos * theta^(-2i/d))
  *   x_rot[2i+1] = x[2i] * sin(pos * theta^(-2i/d)) + x[2i+1] * cos(pos * theta^(-2i/d))
  * 
  * Where theta is typically 10000 (or 500000 for Llama 3).
  */
object RoPEProgram:
  val BLOCK_SIZE = 256

  case class Sizes(
    B: Int,
    T: Int,
    numHeads: Int,
    headSize: Int,
    theta: Float,
    startPos: Int = 0,  // Compile-time default, overridden by uniform at runtime
  ):
    def totalElements: Int = B * T * numHeads * headSize
    def totalPairs: Int = B * T * numHeads * (headSize / 2)

  case class ProgramLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32],
    params: GUniform[AttentionParams],  // Runtime startPos
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val numHeads = sizes.numHeads
    val headSize = sizes.headSize
    val theta = sizes.theta

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float32](s.totalElements),
        output = GBuffer[Float32](s.totalElements),
        params = GUniform[AttentionParams](),
      ),
      dispatch = (_, s) => StaticDispatch(((s.totalPairs + BLOCK_SIZE - 1) / BLOCK_SIZE, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val totalPairsVal: Int32 = B * T * numHeads * (headSize / 2)
      val Tval: Int32 = T
      val numHeadsVal: Int32 = numHeads
      val halfHead: Int32 = headSize / 2
      val thetaVal: Float32 = theta
      // Read startPos from runtime uniform
      val startPosVal: Int32 = layout.params.read.startPos

      GIO.when(idx < totalPairsVal):
        val perHead = halfHead
        val perPos = numHeadsVal * halfHead
        val perBatch = Tval * perPos

        val b = idx / perBatch
        val rem1 = idx.mod(perBatch)
        val t = rem1 / perPos
        val rem2 = rem1.mod(perPos)
        val h = rem2 / perHead
        val d = rem2.mod(perHead)

        val pos = startPosVal + t
        val headSizeFloat: Float32 = headSize.toFloat
        val freqExponent: Float32 = -2.0f * d.asFloat / headSizeFloat
        val freq: Float32 = pos.asFloat * pow(thetaVal, freqExponent)
        val cosFreq = cos(freq)
        val sinFreq = sin(freq)

        val fullIdx: Int32 = b * Tval * numHeadsVal * headSize + t * numHeadsVal * headSize + h * headSize
        val idx0 = fullIdx + d * 2
        val idx1 = idx0 + 1

        val x0 = GIO.read[Float32](layout.input, idx0)
        val x1 = GIO.read[Float32](layout.input, idx1)
        val y0 = x0 * cosFreq - x1 * sinFreq
        val y1 = x0 * sinFreq + x1 * cosFreq

        for
          _ <- GIO.write[Float32](layout.output, idx0, y0)
          _ <- GIO.write[Float32](layout.output, idx1, y1)
        yield Empty()
