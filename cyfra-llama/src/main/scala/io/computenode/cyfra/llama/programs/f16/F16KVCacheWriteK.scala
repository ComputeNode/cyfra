package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Writes K vectors to KV cache at specified positions (F16 version).
  *
  * Each invocation copies one element from the input K tensor to the KV cache
  * at the position specified by the runtime `startPos` parameter.
  *
  * @note The cache is organized as (L × maxSeqLen × NKV × headSize) where L is total layers.
  *       This program writes to a single layer's slice using `cacheLayerOffset`.
  */
object F16KVCacheWriteK:

  /** Compile-time size parameters for the KV cache write K program. */
  case class Sizes(
    B: Int,
    T: Int,
    NKV: Int,
    headSize: Int,
    maxSeqLen: Int,
    layer: Int,
    posOffset: Int,
    cacheLayerOffset: Int,
    L: Int,
  ):
    def totalElements: Int = B * T * NKV * headSize
    def kvSizePerPos: Int = NKV * headSize
    def fullCacheSize: Int = L * maxSeqLen * kvSizePerPos

  case class ProgramLayout(
    k: GBuffer[Float16],
    kCache: GBuffer[Float16],
    params: GUniform[AttentionParams],
  ) derives Layout

  /** Creates a GPU program that writes K vectors to the KV cache. */
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val NKV = sizes.NKV
    val headSize = sizes.headSize
    val totalElements = sizes.totalElements
    val cacheLayerOffset = sizes.cacheLayerOffset
    val kvSizePerPos = sizes.kvSizePerPos
    val fullCacheSize = sizes.fullCacheSize

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        k = GBuffer[Float16](s.B * s.T * s.NKV * s.headSize),
        kCache = GBuffer[Float16](s.fullCacheSize),
        params = GUniform[AttentionParams](),
      ),
      dispatch = (_, s) => StaticDispatch((s.totalElements, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val posOffsetVal: Int32 = layout.params.read.startPos

      val Tval: Int32 = T
      val NKVval: Int32 = NKV
      val headSizeVal: Int32 = headSize
      val totalElementsVal: Int32 = totalElements
      val cacheLayerOffsetVal: Int32 = cacheLayerOffset
      val kvSizePerPosVal: Int32 = kvSizePerPos

      GIO.when(idx < totalElementsVal):
        val elementsPerBatch = Tval * NKVval * headSizeVal
        val b = idx / elementsPerBatch
        val remaining1 = idx.mod(elementsPerBatch)
        val elementsPerPos = NKVval * headSizeVal
        val t = remaining1 / elementsPerPos
        val remaining2 = remaining1.mod(elementsPerPos)
        val h = remaining2 / headSizeVal
        val d = remaining2.mod(headSizeVal)

        val inputIdx = idx
        val kVal = GIO.read[Float16](layout.k, inputIdx)

        val cachePos = posOffsetVal + t
        val cacheIdx = cacheLayerOffsetVal + cachePos * kvSizePerPosVal + h * headSizeVal + d
        GIO.write[Float16](layout.kCache, cacheIdx, kVal)
