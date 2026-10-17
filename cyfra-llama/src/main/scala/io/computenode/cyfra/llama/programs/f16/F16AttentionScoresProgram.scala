package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Attention scores computation: Q · K^T
  *
  * Computes scaled dot-product attention scores for KV-cached inference. One workgroup per (batch, queryPos, head). Each thread handles multiple K
  * positions. Output is F32 scores buffer for subsequent softmax.
  *
  * Matches llama.cpp's mul_mat_vec pattern.
  */
object F16AttentionScoresProgram:
  val BLOCK_SIZE = 32

  case class Sizes(B: Int, T: Int, NH: Int, NKV: Int, headSize: Int, maxSeqLen: Int, kCacheLayerOffset: Int, L: Int):
    def gqaRatio: Int = NH / NKV
    def kvSizePerPos: Int = NKV * headSize
    def fullCacheSize: Int = L * maxSeqLen * kvSizePerPos
    def numIterations: Int = (maxSeqLen + BLOCK_SIZE - 1) / BLOCK_SIZE

  case class ProgramLayout(
    q: GBuffer[RuntimeArray[Float16]],
    kCache: GBuffer[RuntimeArray[Float16]],
    scores: GBuffer[RuntimeArray[Float32]],
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

    GioProgram[Sizes, ProgramLayout](
      layout = s =>
        ProgramLayout(
          q = GBuffer[RuntimeArray[Float16]](s.B * s.T * s.NH * s.headSize),
          kCache = GBuffer[RuntimeArray[Float16]](s.fullCacheSize),
          scores = GBuffer[RuntimeArray[Float32]](s.B * s.T * s.NH * s.maxSeqLen),
          params = GUniform[AttentionParams](),
        ),
      dispatch = (_, s) => StaticDispatch((s.B * s.T * s.NH, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x

      val runtimeParams = GIO.read(layout.params)
      val seqLen: Int32 = runtimeParams.seqLen
      val startPos: Int32 = runtimeParams.startPos

      // Derive batch/pos/head from workgroup ID - compute once
      val batchIdx = GIO.eval(workgroupId / (T * NH))
      val remainder = GIO.eval(workgroupId % (T * NH))
      val queryPosLocal = GIO.eval(remainder / NH)
      val headIdx = GIO.eval(remainder % NH)
      val kvHeadIdx = GIO.eval(headIdx / gqaRatio)

      val queryPosGlobal = GIO.eval(startPos + queryPosLocal)

      // Base addresses - compute once
      val qBase = GIO.eval(batchIdx * (T * NH * headSize) + queryPosLocal * (NH * headSize) + headIdx * headSize)
      val scoresBase = GIO.eval(workgroupId * maxSeqLen)
      val kCacheBase0 = GIO.eval((kCacheLayerOffset: Int32) + kvHeadIdx * headSize) // Partial - add kPos * kvSizePerPos later

      // Each thread handles multiple K positions
      GIO.repeat(numIterations): iter =>
        val kPos: Int32 = tid + iter * BLOCK_SIZE

        // Keep invalid positions outside the dot-product loop, as in the original lazy DSL.
        val isValid = kPos <= queryPosGlobal && kPos < seqLen
        val score: Float32 = GIO.branch(
          isValid, {
            val kCacheBase: Int32 = kCacheBase0 + kPos * kvSizePerPos
            val dot = GIO.foldRepeat[Float32](headSize, 0.0f, unroll = true) { (d, acc) =>
              val qVal = GIO.read(layout.q.focus(_.at(qBase + d))).asFloat32
              val kVal = GIO.read(layout.kCache.focus(_.at(kCacheBase + d))).asFloat32
              acc + qVal * kVal
            }
            dot * scale
          },
          Float32(-10000.0f),
        )
        GIO.write(layout.scores.focus(_.at(scoresBase + kPos)), score)
