package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Layer Normalization forward program.
  *
  * For pipeline use with concatenated parameters (L layers),
  * set L > 1 and the layer index in params to read from correct offset.
  */
object LayerNormProgram:

  val EPS: Float = 1e-5f

  case class LayerNormSizes(B: Int, T: Int, C: Int, L: Int = 1):
    def totalBT: Int = B * T
    def totalElements: Int = B * T * C

  case class LayerNormForwardLayout(
    out: GBuffer[Float32],
    mean: GBuffer[Float32],
    rstd: GBuffer[Float32],
    inp: GBuffer[Float32],
    weight: GBuffer[Float32],  // size L*C for pipeline, C for standalone
    bias: GBuffer[Float32],    // size L*C for pipeline, C for standalone
    params: GUniform[GPT2Params],
  ) derives Layout

  /** Create forward program for a specific layer (layer index baked in) */
  def forward(sizes: LayerNormSizes, layerIdx: Int = 0): GProgram[LayerNormSizes, LayerNormForwardLayout] =
    GProgram[LayerNormSizes, LayerNormForwardLayout](
      layout = s =>
        LayerNormForwardLayout(
          out = GBuffer[Float32](s.totalElements),
          mean = GBuffer[Float32](s.totalBT),
          rstd = GBuffer[Float32](s.totalBT),
          inp = GBuffer[Float32](s.totalElements),
          weight = GBuffer[Float32](s.L * s.C),
          bias = GBuffer[Float32](s.L * s.C),
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
        val c = idx.mod(C)
        val bt = idx / C
        val baseIdx = bt * C

        // Parameter offset for this layer (baked in at compile time as constant)
        val paramOffset: Int32 = (layerIdx * sizes.C): Int32

        val meanVal = GSeq
          .gen[Int32](0, _ + 1)
          .limit(1024)
          .fold(0.0f, (sum: Float32, i: Int32) => {
            when(i < C)(sum + GIO.read[Float32](layout.inp, baseIdx + i)).otherwise(sum)
          }) / C.asFloat

        val variance = GSeq
          .gen[Int32](0, _ + 1)
          .limit(1024)
          .fold(0.0f, (sum: Float32, i: Int32) => {
            when(i < C):
              val x = GIO.read[Float32](layout.inp, baseIdx + i)
              val xshift = x - meanVal
              sum + xshift * xshift
            .otherwise(sum)
          }) / C.asFloat

        val rstdVal = 1.0f / sqrt(variance + EPS)

        val x = GIO.read[Float32](layout.inp, idx)
        val n = (x - meanVal) * rstdVal
        val w = GIO.read[Float32](layout.weight, paramOffset + c)
        val b = GIO.read[Float32](layout.bias, paramOffset + c)
        val result = n * w + b

        for
          _ <- GIO.write(layout.out, idx, result)
          _ <- GIO.when(c === 0):
            for
              _ <- GIO.write(layout.mean, bt, meanVal)
              _ <- GIO.write(layout.rstd, bt, rstdVal)
            yield GStruct.Empty()
        yield GStruct.Empty()
