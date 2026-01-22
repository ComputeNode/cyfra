package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** GELU activation: gelu(x) = 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3))) */
object GeLUProgram:

  val GELU_SCALING_FACTOR: Float = math.sqrt(2.0 / math.Pi).toFloat

  case class GeLUSizes(N: Int)

  case class GeLUForwardLayout(
    out: GBuffer[Float32],
    inp: GBuffer[Float32],
    params: GUniform[GPT2Params],
  ) derives Layout

  private def tanhGpu(x: Float32): Float32 =
    val exp2x = exp(x * 2.0f)
    (exp2x - 1.0f) / (exp2x + 1.0f)

  def forward(sizes: GeLUSizes): GProgram[GeLUSizes, GeLUForwardLayout] =
    GProgram[GeLUSizes, GeLUForwardLayout](
      layout = s =>
        GeLUForwardLayout(
          out = GBuffer[Float32](s.N),
          inp = GBuffer[Float32](s.N),
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
        val x = GIO.read[Float32](layout.inp, idx)
        val cube = 0.044715f * x * x * x
        val tanhArg = GELU_SCALING_FACTOR * (x + cube)
        val result = 0.5f * x * (1.0f + tanhGpu(tanhArg))
        GIO.write(layout.out, idx, result)
