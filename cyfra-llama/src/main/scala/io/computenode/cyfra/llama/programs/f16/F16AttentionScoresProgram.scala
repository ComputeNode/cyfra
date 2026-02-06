package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Attention scores computation: Q · K^T
  *
  * Computes scaled dot-product attention scores for KV-cached inference.
  * One workgroup per (batch, queryPos, head).
  * Each thread handles multiple K positions.
  * Output is F32 scores buffer for subsequent softmax.
  *
  * Matches llama.cpp's mul_mat_vec pattern.
  */
object F16AttentionScoresProgram:
  val BLOCK_SIZE = 32

  case class Sizes(
    B: Int,
    T: Int,
    NH: Int,
    NKV: Int,
    headSize: Int,
    maxSeqLen: Int,
    kCacheLayerOffset: Int,
    L: Int,
  ):
    def gqaRatio: Int = NH / NKV
    def kvSizePerPos: Int = NKV * headSize
    def fullCacheSize: Int = L * maxSeqLen * kvSizePerPos
    def numIterations: Int = (maxSeqLen + BLOCK_SIZE - 1) / BLOCK_SIZE

  case class ProgramLayout(
    q: GBuffer[Float16],
    kCache: GBuffer[Float16],
    scores: GBuffer[Float32],
    params: GUniform[AttentionParams],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    // All compile-time constants
    val B = sizes.B
    val T = sizes.T
    val NH = sizes.NH
    val NKV = sizes.NKV
    val headSize = sizes.headSize
    val gqaRatio = sizes.gqaRatio
    val kCacheLayerOffset = sizes.kCacheLayerOffset
    val kvSizePerPos = sizes.kvSizePerPos
    val maxSeqLen = sizes.maxSeqLen
    val numIterations = sizes.numIterations
    val scale = 1.0f / math.sqrt(headSize).toFloat

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        q = GBuffer[Float16](s.B * s.T * s.NH * s.headSize),
        kCache = GBuffer[Float16](s.fullCacheSize),
        scores = GBuffer[Float32](s.B * s.T * s.NH * s.maxSeqLen),
        params = GUniform[AttentionParams](),
      ),
      dispatch = (_, s) => StaticDispatch((s.B * s.T * s.NH, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x

      val runtimeParams = layout.params.read
      val seqLen: Int32 = runtimeParams.seqLen
      val startPos: Int32 = runtimeParams.startPos

      for
        // Derive batch/pos/head from workgroup ID - compute once
        batchIdx <- GIO.pure(workgroupId / (T * NH))
        remainder <- GIO.pure(workgroupId.mod(T * NH))
        queryPosLocal <- GIO.pure(remainder / NH)
        headIdx <- GIO.pure(remainder.mod(NH))
        kvHeadIdx <- GIO.pure(headIdx / gqaRatio)
        
        queryPosGlobal <- GIO.pure(startPos + queryPosLocal)
        
        // Base addresses - compute once
        qBase <- GIO.pure(batchIdx * (T * NH * headSize) + queryPosLocal * (NH * headSize) + headIdx * headSize)
        scoresBase <- GIO.pure(workgroupId * maxSeqLen)
        kCacheBase0 <- GIO.pure((kCacheLayerOffset: Int32) + kvHeadIdx * headSize) // Partial - add kPos * kvSizePerPos later
        
        // Each thread handles multiple K positions
        _ <- GIO.foldRepeat[GStruct.Empty](numIterations, GStruct.Empty()): (iter, _) =>
          val kPos: Int32 = tid + iter * BLOCK_SIZE
          
          // K cache base for this position
          val kCacheBase: Int32 = kCacheBase0 + kPos * kvSizePerPos
          
          // Compute dot product Q · K[kPos] - unrolled inner loop over headSize
          val dot: Float32 = GSeq.gen[Int32](0, _ + 1).limit(headSize).unroll.fold(0.0f, (acc: Float32, d: Int32) =>
            val qVal: Float32 = GIO.read[Float16](layout.q, qBase + d).asFloat32
            val kVal: Float32 = GIO.read[Float16](layout.kCache, kCacheBase + d).asFloat32
            acc + qVal * kVal
          )
          
          // Scaled score or -inf for invalid (causal masking + bounds)
          val isValid = kPos <= queryPosGlobal && kPos < seqLen
          val score: Float32 = when(isValid)(dot * scale).otherwise(-10000.0f)
          GIO.write[Float32](layout.scores, scoresBase + kPos, score)
      yield GStruct.Empty()
