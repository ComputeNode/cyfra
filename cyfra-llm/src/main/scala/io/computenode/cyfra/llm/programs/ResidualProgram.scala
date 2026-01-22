package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Residual connection: out = inp1 + inp2 */
object ResidualProgram:

  case class ResidualSizes(N: Int)

  case class ResidualForwardLayout(
    out: GBuffer[Float32],
    inp1: GBuffer[Float32],
    inp2: GBuffer[Float32],
    params: GUniform[GPT2Params],
  ) derives Layout

  def forward(sizes: ResidualSizes): GProgram[ResidualSizes, ResidualForwardLayout] =
    GProgram[ResidualSizes, ResidualForwardLayout](
      layout = s =>
        ResidualForwardLayout(
          out = GBuffer[Float32](s.N),
          inp1 = GBuffer[Float32](s.N),
          inp2 = GBuffer[Float32](s.N),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.N + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val N = layout.params.read.N

      GIO.when(idx < N):
        val v1 = GIO.read[Float32](layout.inp1, idx)
        val v2 = GIO.read[Float32](layout.inp2, idx)
        GIO.write(layout.out, idx, v1 + v2)
