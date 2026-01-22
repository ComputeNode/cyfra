package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 element-wise addition for residual connections: `output = a + b`. */
object F16ResidualAddProgram:
  case class Sizes(size: Int)
  
  case class ProgramLayout(
    a: GBuffer[Float16],
    b: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        a = GBuffer[Float16](s.size),
        b = GBuffer[Float16](s.size),
        output = GBuffer[Float16](s.size),
      ),
      dispatch = (_, s) => StaticDispatch(((s.size + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      GIO.when(idx < sizes.size):
        val aVal = GIO.read[Float16](layout.a, idx)
        val bVal = GIO.read[Float16](layout.b, idx)
        GIO.write[Float16](layout.output, idx, aVal + bVal)
