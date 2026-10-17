package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Fused F16 KV Cache Write for both K and V in single dispatch.
  *
  * Reduces dispatch count by writing both K and V vectors to cache simultaneously. Each invocation copies one element from either K or V input to the
  * cache.
  *
  * K cache layout: [layer][pos][head][dim] (standard for Q·K dot products) V cache layout: [layer][head][dim][pos] (TRANSPOSED for coalesced
  * AttentionOutput reads)
  */
object F16FusedKVCacheWriteProgram:

  case class Sizes(
    B: Int,
    T: Int,
    NKV: Int,
    headSize: Int,
    maxSeqLen: Int,
    layer: Int,
    posOffset: Int,
    kCacheLayerOffset: Int,
    vCacheLayerOffset: Int,
    L: Int,
  ):
    def totalKElements: Int = B * T * NKV * headSize
    def totalVElements: Int = B * T * NKV * headSize
    def totalElements: Int = totalKElements + totalVElements
    def kvSizePerPos: Int = NKV * headSize
    def fullCacheSize: Int = L * maxSeqLen * kvSizePerPos

  case class ProgramLayout(
    k: GBuffer[RuntimeArray[Float16]],
    v: GBuffer[RuntimeArray[Float16]],
    kCache: GBuffer[RuntimeArray[Float16]],
    vCache: GBuffer[RuntimeArray[Float16]],
    params: GUniform[AttentionParams],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val NKV = sizes.NKV
    val headSize = sizes.headSize
    val maxSeqLen = sizes.maxSeqLen
    val totalKElements = sizes.totalKElements
    val totalElements = sizes.totalElements
    val kCacheLayerOffset = sizes.kCacheLayerOffset
    val vCacheLayerOffset = sizes.vCacheLayerOffset
    val kvSizePerPos = sizes.kvSizePerPos
    val fullCacheSize = sizes.fullCacheSize

    GioProgram[Sizes, ProgramLayout](
      layout = s =>
        ProgramLayout(
          k = GBuffer[RuntimeArray[Float16]](s.totalKElements),
          v = GBuffer[RuntimeArray[Float16]](s.totalVElements),
          kCache = GBuffer[RuntimeArray[Float16]](s.fullCacheSize),
          vCache = GBuffer[RuntimeArray[Float16]](s.fullCacheSize),
          params = GUniform[AttentionParams](),
        ),
      dispatch = (_, s) => StaticDispatch(((s.totalElements + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val posOffsetVal: Int32 = GIO.read(layout.params).startPos

      val Tval: Int32 = T
      val NKVval: Int32 = NKV
      val headSizeVal: Int32 = headSize
      val maxSeqLenVal: Int32 = maxSeqLen
      val totalKElementsVal: Int32 = totalKElements
      val totalElementsVal: Int32 = totalElements
      val kCacheLayerOffsetVal: Int32 = kCacheLayerOffset
      val vCacheLayerOffsetVal: Int32 = vCacheLayerOffset
      val kvSizePerPosVal: Int32 = kvSizePerPos

      GIO.when(idx < totalElementsVal):
        // Determine if this is K or V
        val isK = idx < totalKElementsVal
        val localIdx: Int32 = GIO.branch(isK, idx, idx - totalKElementsVal)

        // Decompose index
        val elementsPerBatch = Tval * NKVval * headSizeVal
        val b = localIdx / elementsPerBatch
        val remaining1 = localIdx % elementsPerBatch
        val elementsPerPos = NKVval * headSizeVal
        val t = remaining1 / elementsPerPos
        val remaining2 = remaining1 % elementsPerPos
        val h = remaining2 / headSizeVal
        val d = remaining2 % headSizeVal

        val cachePos = posOffsetVal + t
        // K cache: [layer][pos][head][dim] - standard layout
        val kCacheOffset: Int32 = cachePos * kvSizePerPosVal + h * headSizeVal + d
        // V cache: [layer][head][dim][pos] - TRANSPOSED for coalesced reads
        val vCacheOffset: Int32 = h * headSizeVal * maxSeqLenVal + d * maxSeqLenVal + cachePos

        GIO.when(isK):
          val kVal = GIO.read(layout.k.focus(_.at(localIdx)))
          val cacheIdx = kCacheLayerOffsetVal + kCacheOffset
          GIO.write(layout.kCache.focus(_.at(cacheIdx)), kVal)
        GIO.when(!isK):
          val vVal = GIO.read(layout.v.focus(_.at(localIdx)))
          val cacheIdx = vCacheLayerOffsetVal + vCacheOffset
          GIO.write(layout.vCache.focus(_.at(cacheIdx)), vVal)
