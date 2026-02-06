package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Writes V vectors to KV cache at specified positions (F16 version).
  *
  * V cache is stored TRANSPOSED for coalesced reads in AttentionOutput:
  *   Layout: [layer][head][dim][pos] instead of [layer][pos][head][dim]
  * 
  * This allows consecutive threads reading consecutive positions to access
  * consecutive memory locations, achieving ~100% memory bandwidth efficiency.
  */
object F16KVCacheWriteV:

  /** Compile-time size parameters for the KV cache write V program. */
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
    v: GBuffer[Float16],
    vCache: GBuffer[Float16],
    params: GUniform[AttentionParams],
  ) derives Layout

  /** Creates a GPU program that writes V vectors to the KV cache.
    * 
    * V cache is transposed: [layer][head][dim][pos]
    * Index formula: layerOffset + head * headSize * maxSeqLen + dim * maxSeqLen + pos
    */
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val NKV = sizes.NKV
    val headSize = sizes.headSize
    val maxSeqLen = sizes.maxSeqLen
    val totalElements = sizes.totalElements
    val cacheLayerOffset = sizes.cacheLayerOffset
    val fullCacheSize = sizes.fullCacheSize

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        v = GBuffer[Float16](s.B * s.T * s.NKV * s.headSize),
        vCache = GBuffer[Float16](s.fullCacheSize),
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
      val maxSeqLenVal: Int32 = maxSeqLen
      val totalElementsVal: Int32 = totalElements
      val cacheLayerOffsetVal: Int32 = cacheLayerOffset

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
        val vVal = GIO.read[Float16](layout.v, inputIdx)

        val cachePos = posOffsetVal + t
        // TRANSPOSED: [layer][head][dim][pos] for coalesced reads in AttentionOutput
        val cacheIdx = cacheLayerOffsetVal + h * headSizeVal * maxSeqLenVal + d * maxSeqLenVal + cachePos
        GIO.write[Float16](layout.vCache, cacheIdx, vVal)
