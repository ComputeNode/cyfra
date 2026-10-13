package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO

/** F16 element-wise addition for residual connections: `output = a + b`. */
object F16ResidualAddProgram:
  case class Sizes(size: Int)

  case class ProgramLayout(a: GBuffer[GArray[Float16]], b: GBuffer[GArray[Float16]], output: GBuffer[GArray[Float16]]) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    GioProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(a = GBuffer[GArray[Float16]](s.size), b = GBuffer[GArray[Float16]](s.size), output = GBuffer[GArray[Float16]](s.size)),
      dispatch = (_, s) => StaticDispatch(((s.size + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      GIO.when(idx < sizes.size):
        val aVal = GIO.read(layout.a.focus(_.at(idx)))
        val bVal = GIO.read(layout.b.focus(_.at(idx)))
        GIO.write(layout.output.focus(_.at(idx)), aVal + bVal)
