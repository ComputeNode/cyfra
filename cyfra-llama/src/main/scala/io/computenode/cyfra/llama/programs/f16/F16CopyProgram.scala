package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 buffer copy. Used to save state for residual connections. */
object F16CopyProgram:
  case class Sizes(size: Int)
  
  case class ProgramLayout(
    input: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float16](s.size),
        output = GBuffer[Float16](s.size),
      ),
      dispatch = (_, s) => StaticDispatch(((s.size + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      GIO.when(idx < sizes.size):
        val value = GIO.read[Float16](layout.input, idx)
        GIO.write[Float16](layout.output, idx, value)
