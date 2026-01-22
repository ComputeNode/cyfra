package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** AdamW optimizer update program.
  *
  * AdamW update rule:
  *   m = beta1 * m + (1 - beta1) * grad
  *   v = beta2 * v + (1 - beta2) * grad^2
  *   m_hat = m / (1 - beta1^t)
  *   v_hat = v / (1 - beta2^t)
  *   param = param - lr * (m_hat / (sqrt(v_hat) + eps) + weight_decay * param)
  */
object AdamWProgram:

  /** AdamW hyperparameters */
  case class AdamWParams(
    numParams: Int32,
    learningRate: Float32,
    beta1: Float32,
    beta2: Float32,
    eps: Float32,
    weightDecay: Float32,
    beta1PowT: Float32, // beta1^t (precomputed)
    beta2PowT: Float32, // beta2^t (precomputed)
  ) extends GStruct[AdamWParams]

  /** Layout for AdamW update */
  case class AdamWLayout(
    params: GBuffer[Float32],  // model parameters
    grads: GBuffer[Float32],   // gradients
    m: GBuffer[Float32],       // first moment (momentum)
    v: GBuffer[Float32],       // second moment (RMSprop)
    hyperparams: GUniform[AdamWParams],
  ) derives Layout

  /** AdamW update.
    *
    * Each thread updates one parameter.
    */
  val update: GProgram[Int, AdamWLayout] =
    GProgram[Int, AdamWLayout](
      layout = n =>
        AdamWLayout(
          params = GBuffer[Float32](n),
          grads = GBuffer[Float32](n),
          m = GBuffer[Float32](n),
          v = GBuffer[Float32](n),
          hyperparams = GUniform[AdamWParams](),
        ),
      dispatch = (_, n) => {
        val workgroupSize = 256
        val numWorkgroups = (n + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val hp = layout.hyperparams.read
      val N = hp.numParams
      val lr = hp.learningRate
      val beta1 = hp.beta1
      val beta2 = hp.beta2
      val eps = hp.eps
      val wd = hp.weightDecay
      val beta1PowT = hp.beta1PowT
      val beta2PowT = hp.beta2PowT

      GIO.when(idx < N):
        val param = GIO.read[Float32](layout.params, idx)
        val grad = GIO.read[Float32](layout.grads, idx)
        val mOld = GIO.read[Float32](layout.m, idx)
        val vOld = GIO.read[Float32](layout.v, idx)

        // Update first moment (momentum)
        val mNew = beta1 * mOld + (1.0f - beta1) * grad

        // Update second moment (RMSprop)
        val vNew = beta2 * vOld + (1.0f - beta2) * grad * grad

        // Bias correction
        val mHat = mNew / (1.0f - beta1PowT)
        val vHat = vNew / (1.0f - beta2PowT)

        // Update parameter with weight decay (AdamW style)
        val paramNew = param - lr * (mHat / (sqrt(vHat) + eps) + wd * param)

        // Store updated values
        for
          _ <- GIO.write(layout.m, idx, mNew)
          _ <- GIO.write(layout.v, idx, vNew)
          _ <- GIO.write(layout.params, idx, paramNew)
        yield GStruct.Empty()
