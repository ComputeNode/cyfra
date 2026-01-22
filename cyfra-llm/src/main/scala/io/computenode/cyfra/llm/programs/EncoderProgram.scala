package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Encoder: out[b,t,:] = wte[inp[b,t],:] + wpe[t,:] */
object EncoderProgram:

  case class EncoderSizes(B: Int, T: Int, C: Int, Vp: Int, maxT: Int):
    def totalElements: Int = B * T * C

  case class EncoderForwardLayout(
    out: GBuffer[Float32],
    inp: GBuffer[Int32],
    wte: GBuffer[Float32],
    wpe: GBuffer[Float32],
    params: GUniform[GPT2Params],
  ) derives Layout

  def forward(sizes: EncoderSizes): GProgram[EncoderSizes, EncoderForwardLayout] =
    GProgram[EncoderSizes, EncoderForwardLayout](
      layout = s =>
        EncoderForwardLayout(
          out = GBuffer[Float32](s.totalElements),
          inp = GBuffer[Int32](s.B * s.T),
          wte = GBuffer[Float32](s.Vp * s.C),
          wpe = GBuffer[Float32](s.maxT * s.C),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalElements + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val C = p.C
      val totalElements = B * T * C

      GIO.when(idx < totalElements):
        val i = idx.mod(C)
        val t = (idx / C).mod(T)
        val b = idx / (T * C)

        val tokenIdx = GIO.read[Int32](layout.inp, b * T + t)
        val wteVal = GIO.read[Float32](layout.wte, tokenIdx * C + i)
        val wpeVal = GIO.read[Float32](layout.wpe, t * C + i)

        GIO.write(layout.out, idx, wteVal + wpeVal)
