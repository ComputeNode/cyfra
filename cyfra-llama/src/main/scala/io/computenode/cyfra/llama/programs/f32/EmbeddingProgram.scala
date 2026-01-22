package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Embedding lookup program: output = embeddings[tokens] */
object EmbeddingProgram:
  val BLOCK_SIZE = 256

  case class Sizes(
    seqLen: Int,
    hiddenSize: Int,
    vocabSize: Int,
  ):
    def totalOutputs: Int = seqLen * hiddenSize
    def embeddingSize: Int = vocabSize * hiddenSize

  case class ProgramLayout(
    tokens: GBuffer[Int32],
    embeddings: GBuffer[Float32],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val seqLen = sizes.seqLen
    val hiddenSize = sizes.hiddenSize

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        tokens = GBuffer[Int32](s.seqLen),
        embeddings = GBuffer[Float32](s.embeddingSize),
        output = GBuffer[Float32](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch(((s.totalOutputs + BLOCK_SIZE - 1) / BLOCK_SIZE, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val hiddenSizeVal: Int32 = hiddenSize
      val totalVal: Int32 = seqLen * hiddenSize

      GIO.when(idx < totalVal):
        val tokenPos = idx / hiddenSizeVal
        val dim = idx.mod(hiddenSizeVal)
        val tokenId = GIO.read[Int32](layout.tokens, tokenPos)
        val embIdx = tokenId * hiddenSizeVal + dim
        val value = GIO.read[Float32](layout.embeddings, embIdx)
        GIO.write[Float32](layout.output, idx, value)
