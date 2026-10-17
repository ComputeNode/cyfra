package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO

/** F16 token embedding lookup.
  *
  * Maps token IDs to embedding vectors by index lookup.
  */
object F16EmbeddingProgram:
  case class Sizes(seqLen: Int, hiddenSize: Int, vocabSize: Int):
    def totalOutputs: Int = seqLen * hiddenSize

  case class ProgramLayout(tokens: GBuffer[RuntimeArray[Int32]], embeddings: GBuffer[RuntimeArray[Float16]], output: GBuffer[RuntimeArray[Float16]])
      derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    GioProgram[Sizes, ProgramLayout](
      layout = s =>
        ProgramLayout(
          tokens = GBuffer[RuntimeArray[Int32]](s.seqLen),
          embeddings = GBuffer[RuntimeArray[Float16]](s.vocabSize * s.hiddenSize),
          output = GBuffer[RuntimeArray[Float16]](s.totalOutputs),
        ),
      dispatch = (_, s) => StaticDispatch(((s.totalOutputs + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val hiddenSizeVal: Int32 = sizes.hiddenSize
      val totalVal: Int32 = sizes.seqLen * sizes.hiddenSize

      GIO.when(idx < totalVal):
        val tokenPos = idx / hiddenSizeVal
        val dim = idx % hiddenSizeVal
        val tokenId = GIO.read(layout.tokens.focus(_.at(tokenPos)))
        val embIdx = tokenId * hiddenSizeVal + dim
        val value = GIO.read(layout.embeddings.focus(_.at(embIdx)))
        GIO.write(layout.output.focus(_.at(idx)), value)
