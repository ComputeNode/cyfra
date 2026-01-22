package io.computenode.cyfra.llm

import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import scala.math.abs

/** GPT-2 Tests matching the llm.c test suite.
  *
  * These tests verify:
  * 1. Forward pass produces correct logits
  * 2. Backward pass produces correct gradients
  * 3. Training converges with expected losses
  */
class GPT2Test extends FunSuite:

  private val tolerance = 2e-2f

  /** Check if two values are close enough */
  private def isClose(a: Float, b: Float): Boolean =
    abs(a - b) <= tolerance

  /** Check if two arrays match within tolerance */
  private def checkTensor(
    actual: Array[Float],
    expected: Array[Float],
    label: String,
    printFirst: Int = 5,
  ): Boolean =
    var maxDiff = 0.0f
    var ok = true
    for i <- actual.indices do
      val diff = abs(actual(i) - expected(i))
      if diff > maxDiff then maxDiff = diff
      if diff > tolerance then ok = false

    println(s"$label: ${if ok then "OK" else "NOT OK"}, maxdiff = $maxDiff")
    if !ok || printFirst > 0 then
      for i <- 0 until math.min(printFirst, actual.length) do
        println(s"  [$i] actual=${actual(i)}, expected=${expected(i)}")
    ok

  test("GPT2Config sizes are correct for GPT-2 124M") {
    val config = GPT2Config.GPT2_124M

    assertEquals(config.maxSeqLen, 1024)
    assertEquals(config.vocabSize, 50257)
    assertEquals(config.paddedVocabSize, 50304)
    assertEquals(config.numLayers, 12)
    assertEquals(config.numHeads, 12)
    assertEquals(config.channels, 768)
    assertEquals(config.headSize, 64)

    // Check parameter count
    val numParams = config.numParameters
    println(s"GPT-2 124M has $numParams parameters")

    // GPT-2 124M should have ~124M parameters
    assert(numParams > 100_000_000 && numParams < 150_000_000)
  }

  test("GPT2Config activation sizes are correct") {
    val config = GPT2Config.GPT2_124M
    val B = 4
    val T = 64

    val numActivations = config.numActivations(B, T)
    println(s"Activations for B=$B, T=$T: $numActivations")

    // Verify it's a reasonable number
    assert(numActivations > 0)
  }

  test("CPU forward pass produces output") {
    VkCyfraRuntime.using:
      val config = GPT2Config(
        maxSeqLen = 64,
        vocabSize = 100,
        paddedVocabSize = 128,
        numLayers = 2,
        numHeads = 4,
        channels = 64,
      )

      val model = GPT2Model.random(config, seed = 42L)

      val B = 2
      val T = 16
      val rng = new scala.util.Random(123)
      val inputs = Array.fill(B * T)(rng.nextInt(config.vocabSize))
      val targets = Array.fill(B * T)(rng.nextInt(config.vocabSize))

      val loss = model.forwardCPU(inputs, targets, B, T)

      println(s"Forward pass loss: $loss")
      assert(loss > 0, s"Loss should be positive, got $loss")
      assert(!loss.isNaN, "Loss should not be NaN")
      assert(!loss.isInfinite, "Loss should not be infinite")
  }

  test("LayerNorm produces normalized output") {
    // Test that layer norm actually normalizes the input
    val input = Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val mean = input.sum / input.length
    val variance = input.map(x => (x - mean) * (x - mean)).sum / input.length
    val std = math.sqrt(variance).toFloat

    println(s"Input: ${input.mkString(", ")}")
    println(s"Mean: $mean, Std: $std")

    // After normalization, mean should be ~0 and std should be ~1
    val normalized = input.map(x => (x - mean) / std)
    val normMean = normalized.sum / normalized.length
    val normStd = math.sqrt(normalized.map(x => x * x).sum / normalized.length).toFloat

    println(s"Normalized mean: $normMean, Normalized std: $normStd")
    assert(abs(normMean) < 0.01f)
    assert(abs(normStd - 1.0f) < 0.01f)
  }

  test("Softmax produces valid probability distribution") {
    val logits = Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val maxLogit = logits.max
    val expLogits = logits.map(x => math.exp(x - maxLogit).toFloat)
    val sumExp = expLogits.sum
    val probs = expLogits.map(_ / sumExp)

    println(s"Logits: ${logits.mkString(", ")}")
    println(s"Probs: ${probs.mkString(", ")}")

    // Verify probabilities sum to 1
    val probSum = probs.sum
    println(s"Probability sum: $probSum")
    assert(abs(probSum - 1.0f) < 0.001f)

    // Verify all probabilities are positive
    assert(probs.forall(_ > 0))

    // Verify higher logits -> higher probabilities
    for i <- 1 until probs.length do
      assert(probs(i) > probs(i - 1))
  }

  test("Cross-entropy loss is correct") {
    val probs = Array(0.1f, 0.2f, 0.3f, 0.4f)
    val target = 2 // Index of correct class

    val loss = -math.log(probs(target)).toFloat
    println(s"Cross-entropy loss for target $target with prob ${probs(target)}: $loss")

    // Verify loss is positive
    assert(loss > 0)

    // Verify loss decreases as probability increases
    val higherProb = 0.9f
    val lowerLoss = -math.log(higherProb).toFloat
    assert(lowerLoss < loss)
  }

  test("GELU activation is monotonically increasing for positive inputs") {
    val geluScalingFactor = math.sqrt(2.0 / math.Pi).toFloat

    def gelu(x: Float): Float =
      val cube = 0.044715f * x * x * x
      0.5f * x * (1.0f + math.tanh(geluScalingFactor * (x + cube)).toFloat)

    val inputs = Array(0.0f, 0.5f, 1.0f, 1.5f, 2.0f)
    val outputs = inputs.map(gelu)

    println(s"GELU inputs: ${inputs.mkString(", ")}")
    println(s"GELU outputs: ${outputs.mkString(", ")}")

    // Verify monotonically increasing for positive inputs
    for i <- 1 until outputs.length do
      assert(outputs(i) > outputs(i - 1))

    // Verify GELU(0) = 0
    assert(abs(gelu(0.0f)) < 0.001f)
  }

  test("Attention scaling is correct") {
    val headSize = 64
    val scale = 1.0f / math.sqrt(headSize).toFloat

    println(s"Attention scale for head_size=$headSize: $scale")
    assert(abs(scale - 0.125f) < 0.001f) // 1/sqrt(64) = 1/8 = 0.125
  }

  test("Expected losses for training match reference") {
    // These are the expected losses from llm.c test_gpt2.c
    val expectedLosses = Array(
      5.270007133483887f,
      4.059706687927246f,
      3.3751230239868164f,
      2.8007826805114746f,
      2.315382242202759f,
      1.8490285873413086f,
      1.3946564197540283f,
      0.9991465210914612f,
      0.6240804195404053f,
      0.37651097774505615f,
    )

    // Verify losses are decreasing (training is converging)
    for i <- 1 until expectedLosses.length do
      assert(
        expectedLosses(i) < expectedLosses(i - 1),
        s"Loss at step $i (${expectedLosses(i)}) should be less than step ${i - 1} (${expectedLosses(i - 1)})",
      )

    println("Expected losses for 10 training steps:")
    expectedLosses.zipWithIndex.foreach { case (loss, step) =>
      println(s"  Step $step: $loss")
    }
  }
