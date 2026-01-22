package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Softmax and Cross-Entropy programs. */
object SoftmaxCrossEntropyProgram:

  case class SoftmaxSizes(B: Int, T: Int, V: Int, Vp: Int):
    def totalBT: Int = B * T

  case class CrossEntropySizes(B: Int, T: Int, Vp: Int):
    def totalBT: Int = B * T

  case class SoftmaxForwardLayout(
    probs: GBuffer[Float32],
    logits: GBuffer[Float32],
    params: GUniform[GPT2Params],
  ) derives Layout

  case class CrossEntropyForwardLayout(
    losses: GBuffer[Float32],
    probs: GBuffer[Float32],
    targets: GBuffer[Int32],
    params: GUniform[GPT2Params],
  ) derives Layout

  def softmaxForward(sizes: SoftmaxSizes): GProgram[SoftmaxSizes, SoftmaxForwardLayout] =
    GProgram[SoftmaxSizes, SoftmaxForwardLayout](
      layout = s =>
        SoftmaxForwardLayout(
          probs = GBuffer[Float32](s.B * s.T * s.Vp),
          logits = GBuffer[Float32](s.B * s.T * s.Vp),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalBT + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val btIdx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val V = p.V
      val Vp = p.Vp
      val totalBT = B * T

      GIO.when(btIdx < totalBT):
        val base = btIdx * Vp

        val maxVal = GSeq
          .gen[Int32](0, _ + 1)
          .limit(65536)
          .fold(-10000.0f, (curMax: Float32, i: Int32) => {
            when(i < V)(max(curMax, GIO.read[Float32](layout.logits, base + i))).otherwise(curMax)
          })

        val expSum = GSeq
          .gen[Int32](0, _ + 1)
          .limit(65536)
          .fold(0.0f, (sum: Float32, i: Int32) => {
            when(i < V)(sum + exp(GIO.read[Float32](layout.logits, base + i) - maxVal)).otherwise(sum)
          })

        GIO.repeat(Vp): i =>
          val probVal = when(i < V):
            exp(GIO.read[Float32](layout.logits, base + i) - maxVal) / expSum
          .otherwise(0.0f)
          GIO.write(layout.probs, base + i, probVal)

  def crossEntropyForward(sizes: CrossEntropySizes): GProgram[CrossEntropySizes, CrossEntropyForwardLayout] =
    GProgram[CrossEntropySizes, CrossEntropyForwardLayout](
      layout = s =>
        CrossEntropyForwardLayout(
          losses = GBuffer[Float32](s.totalBT),
          probs = GBuffer[Float32](s.B * s.T * s.Vp),
          targets = GBuffer[Int32](s.totalBT),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalBT + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val btIdx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val Vp = p.Vp
      val totalBT = B * T

      GIO.when(btIdx < totalBT):
        val targetIdx = GIO.read[Int32](layout.targets, btIdx)
        val probVal = GIO.read[Float32](layout.probs, btIdx * Vp + targetIdx)
        val loss = -logn(probVal)  // natural log
        GIO.write(layout.losses, btIdx, loss)
