package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Residual add program: output = a + b */
object ResidualAddProgram:
  val BLOCK_SIZE = 512

  case class Sizes(size: Int)

  case class ProgramLayout(
    a: GBuffer[Float32],
    b: GBuffer[Float32],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val size = sizes.size

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        a = GBuffer[Float32](s.size),
        b = GBuffer[Float32](s.size),
        output = GBuffer[Float32](s.size),
      ),
      dispatch = (_, s) => StaticDispatch(((s.size + BLOCK_SIZE - 1) / BLOCK_SIZE, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val sizeVal: Int32 = size

      GIO.when(idx < sizeVal):
        val aVal = GIO.read[Float32](layout.a, idx)
        val bVal = GIO.read[Float32](layout.b, idx)
        GIO.write[Float32](layout.output, idx, aVal + bVal)

/** Copy program: output = input */
object CopyProgram:
  val BLOCK_SIZE = 512

  case class Sizes(size: Int)

  case class ProgramLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val size = sizes.size

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float32](s.size),
        output = GBuffer[Float32](s.size),
      ),
      dispatch = (_, s) => StaticDispatch(((s.size + BLOCK_SIZE - 1) / BLOCK_SIZE, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val sizeVal: Int32 = size

      GIO.when(idx < sizeVal):
        val v = GIO.read[Float32](layout.input, idx)
        GIO.write[Float32](layout.output, idx, v)
