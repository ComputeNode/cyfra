package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GShared
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.llama.programs.AttentionParams

/** KV-cached attention for incremental inference (F32 version).
  *
  * Computes attention by reading Q from current tokens and K/V from the full cache.
  * One workgroup handles one (batch, query_position, head) tuple.
  * Supports grouped-query attention (GQA) where multiple Q heads share K/V heads.
  */
object KVCachedAttention:
  val WARP_SIZE = 32
  val MAX_SEQ_LEN = 2048

  /** Compile-time size parameters for the attention program. */
  case class Sizes(
    B: Int,
    T: Int,
    NH: Int,
    NKV: Int,
    headSize: Int,
    startPos: Int,
    kCacheLayerOffset: Int,
    vCacheLayerOffset: Int,
    L: Int,
    maxSeqLen: Int,
  ):
    def gqaRatio: Int = NH / NKV
    def numScoreIterations: Int = (maxSeqLen + WARP_SIZE - 1) / WARP_SIZE
    def kvSizePerPos: Int = NKV * headSize
    def fullCacheSize: Int = L * maxSeqLen * kvSizePerPos

  case class ProgramLayout(
    q: GBuffer[Float32],
    kCache: GBuffer[Float32],
    vCache: GBuffer[Float32],
    output: GBuffer[Float32],
    params: GUniform[AttentionParams],
  ) derives Layout

  /** Creates a GPU program for KV-cached attention computation. */
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val scoresShared = GShared[Float32](MAX_SEQ_LEN)

    val B = sizes.B
    val T = sizes.T
    val NH = sizes.NH
    val NKV = sizes.NKV
    val headSize = sizes.headSize
    val gqaRatio = sizes.gqaRatio
    val scale = 1.0f / math.sqrt(headSize).toFloat
    val numScoreIterations = sizes.numScoreIterations
    val kCacheLayerOffset = sizes.kCacheLayerOffset
    val vCacheLayerOffset = sizes.vCacheLayerOffset
    val kvSizePerPos = sizes.kvSizePerPos
    val fullCacheSize = sizes.fullCacheSize
    val maxSeqLen = sizes.maxSeqLen

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        q = GBuffer[Float32](s.B * s.T * s.NH * s.headSize),
        kCache = GBuffer[Float32](s.fullCacheSize),
        vCache = GBuffer[Float32](s.fullCacheSize),
        output = GBuffer[Float32](s.B * s.T * s.NH * s.headSize),
        params = GUniform[AttentionParams](),
      ),
      dispatch = (_, s) => StaticDispatch((s.B * s.T * s.NH, 1, 1)),
      workgroupSize = (WARP_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x

      val runtimeParams = layout.params.read
      val seqLenVal: Int32 = runtimeParams.seqLen
      val startPosVal: Int32 = runtimeParams.startPos

      val Tval: Int32 = T
      val NHval: Int32 = NH
      val NKVval: Int32 = NKV
      val headSizeVal: Int32 = headSize
      val gqaRatioVal: Int32 = gqaRatio
      val scaleVal: Float32 = scale
      val kCacheLayerOffsetVal: Int32 = kCacheLayerOffset
      val vCacheLayerOffsetVal: Int32 = vCacheLayerOffset
      val kvSizePerPosVal: Int32 = kvSizePerPos

      val posPerBatch = Tval * NHval
      val batchIdx = workgroupId / posPerBatch
      val posInBatch = workgroupId.mod(posPerBatch)
      val queryPosLocal = posInBatch / NHval
      val headIdx = posInBatch.mod(NHval)
      val kvHeadIdx = headIdx / gqaRatioVal

      val queryPosGlobal = startPosVal + queryPosLocal
      val qBase = batchIdx * Tval * NHval * headSizeVal + queryPosLocal * NHval * headSizeVal + headIdx * headSizeVal

      // Phase 1: Compute attention scores Q·K and track max for numerical stability
      val computeScoresAndMax: GIO[Float32] = GIO.foldRepeat[Float32](numScoreIterations, -10000.0f): (iter, localMax) =>
        val kPos = tid + iter * WARP_SIZE
        val isValid = kPos <= queryPosGlobal && kPos < seqLenVal
        val kCacheBase = kCacheLayerOffsetVal + kPos * kvSizePerPosVal + kvHeadIdx * headSizeVal

        val dot = GSeq.gen[Int32](0, _ + 1).limit(headSize).fold(0.0f, (acc: Float32, d: Int32) =>
          val qVal = GIO.read[Float32](layout.q, qBase + d)
          val kVal = GIO.read[Float32](layout.kCache, kCacheBase + d)
          acc + qVal * kVal
        )

        val score = when(isValid)(dot * scaleVal).otherwise(-10000.0f)

        for _ <- scoresShared.write(kPos, score) yield
          when(isValid)(max(localMax, score)).otherwise(localMax)

      for
        localMax <- computeScoresAndMax
        _ <- GIO.barrier

        // Phase 2: Compute softmax numerator exp(score - max)
        globalMax <- GIO.pure(GIO.subgroupMax(localMax))
        localSum <- GIO.foldRepeat[Float32](numScoreIterations, 0.0f): (iter, sum) =>
          val kPos = tid + iter * WARP_SIZE
          val isValid = kPos <= queryPosGlobal && kPos < seqLenVal
          val score = scoresShared.read(kPos)
          val expScore = exp(score - globalMax)
          for _ <- scoresShared.write(kPos, expScore) yield
            when(isValid)(sum + expScore).otherwise(sum)

        _ <- GIO.barrier

        // Phase 3: Normalize to get attention weights
        globalSum <- GIO.pure(GIO.subgroupAdd(localSum) + 0.0000001f)
        _ <- GIO.foldRepeat[Empty](numScoreIterations, Empty()): (iter, _) =>
          val kPos = tid + iter * WARP_SIZE
          val isValid = kPos <= queryPosGlobal && kPos < seqLenVal
          val expScore = scoresShared.read(kPos)
          for _ <- GIO.when(isValid)(scoresShared.write(kPos, expScore / globalSum)) yield Empty()

        _ <- GIO.barrier

        // Phase 4: Compute weighted sum of V values
        outDim1 <- GIO.pure(tid)
        _ <- GIO.when(outDim1 < headSizeVal):
          val weightedSum1 = GSeq.gen[Int32](0, _ + 1).limit(maxSeqLen).takeWhile(_ < seqLenVal).fold(0.0f, (sum: Float32, kPos: Int32) =>
            val isValid = kPos <= queryPosGlobal
            val weight = scoresShared.read(kPos)
            val vCacheBase = vCacheLayerOffsetVal + kPos * kvSizePerPosVal + kvHeadIdx * headSizeVal
            val vVal = GIO.read[Float32](layout.vCache, vCacheBase + outDim1)
            when(isValid)(sum + weight * vVal).otherwise(sum)
          )
          val outBase = batchIdx * Tval * NHval * headSizeVal + queryPosLocal * NHval * headSizeVal + headIdx * headSizeVal
          GIO.write[Float32](layout.output, outBase + outDim1, weightedSum1)

        // Handle dimensions beyond WARP_SIZE (for headSize > 32)
        outDim2 <- GIO.pure(tid + WARP_SIZE)
        _ <- GIO.when(outDim2 < headSizeVal):
          val weightedSum2 = GSeq.gen[Int32](0, _ + 1).limit(maxSeqLen).takeWhile(_ < seqLenVal).fold(0.0f, (sum: Float32, kPos: Int32) =>
            val isValid = kPos <= queryPosGlobal
            val weight = scoresShared.read(kPos)
            val vCacheBase = vCacheLayerOffsetVal + kPos * kvSizePerPosVal + kvHeadIdx * headSizeVal
            val vVal = GIO.read[Float32](layout.vCache, vCacheBase + outDim2)
            when(isValid)(sum + weight * vVal).otherwise(sum)
          )
          val outBase = batchIdx * Tval * NHval * headSizeVal + queryPosLocal * NHval * headSizeVal + headIdx * headSizeVal
          GIO.write[Float32](layout.output, outBase + outDim2, weightedSum2)
      yield Empty()
