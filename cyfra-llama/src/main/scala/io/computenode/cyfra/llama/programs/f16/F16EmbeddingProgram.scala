package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.{*, given}
import io.computenode.cyfra.core.GProgram.{*, given}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 token embedding lookup.
  *
  * Maps token IDs to embedding vectors by index lookup.
  */
object F16EmbeddingProgram:
  case class Sizes(seqLen: Int, hiddenSize: Int, vocabSize: Int):
    def totalOutputs: Int = seqLen * hiddenSize
  
  case class ProgramLayout(
    tokens: GBuffer[Int32],
    embeddings: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    given Sizes = sizes
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        tokens = GBuffer.sized[Int32](s.seqLen),
        embeddings = GBuffer.sized[Float16](s.vocabSize * s.hiddenSize),
        output = GBuffer.sized[Float16](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch(((s.totalOutputs + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val hiddenSizeVal: Int32 = sizes.hiddenSize
      val totalVal: Int32 = sizes.seqLen * sizes.hiddenSize
      
      GIO.when(idx < totalVal):
        val tokenPos = idx / hiddenSizeVal
        val dim = idx.mod(hiddenSizeVal)
        val tokenId = GIO.read[Int32](layout.tokens, tokenPos)
        val embIdx = tokenId * hiddenSizeVal + dim
        val value = GIO.read[Float16](layout.embeddings, embIdx)
        GIO.write[Float16](layout.output, idx, value)
