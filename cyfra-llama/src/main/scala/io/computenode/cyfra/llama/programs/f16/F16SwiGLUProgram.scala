package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 SwiGLU activation: `SiLU(gate) * up`.
  *
  * Combines gated linear unit with SiLU (swish) activation.
  * Computes in F32 internally for precision.
  */
object F16SwiGLUProgram:
  case class Sizes(numElements: Int)
  
  case class ProgramLayout(
    gate: GBuffer[Float16],
    up: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        gate = GBuffer[Float16](s.numElements),
        up = GBuffer[Float16](s.numElements),
        output = GBuffer[Float16](s.numElements),
      ),
      dispatch = (_, s) => StaticDispatch(((s.numElements + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val tid = GIO.invocationId
      GIO.when(tid < sizes.numElements):
        val g = GIO.read[Float16](layout.gate, tid).asFloat32
        val u = GIO.read[Float16](layout.up, tid).asFloat32
        val sigmoidG = 1.0f / (1.0f + exp(-g))
        val result = g * sigmoidG * u
        GIO.write[Float16](layout.output, tid, result.asFloat16)
