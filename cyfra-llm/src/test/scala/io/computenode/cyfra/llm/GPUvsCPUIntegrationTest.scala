package io.computenode.cyfra.llm

import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import scala.concurrent.duration.Duration
import scala.math.abs

/** Integration tests comparing GPU forward pass to CPU reference.
  *
  * These tests verify that the GPU pipeline produces results matching
  * the CPU implementation, and measure performance differences.
  */
class GPUvsCPUIntegrationTest extends FunSuite:

  override def munitTimeout: Duration = Duration("10 minutes")

  private def maxError(actual: Array[Float], expected: Array[Float]): Float =
    actual.zip(expected).map { case (a, e) => abs(a - e) }.max

  private def meanError(actual: Array[Float], expected: Array[Float]): Float =
    actual.zip(expected).map { case (a, e) => abs(a - e) }.sum / actual.length

  test("GPU forward pass matches CPU forward pass - small model") {
    VkCyfraRuntime.using:
      // Small model for fast testing
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

      println(s"\n=== Small Model Integration Test ===")
      println(s"Config: ${config.numLayers} layers, ${config.channels} channels, ${config.numHeads} heads")
      println(s"Batch: B=$B, T=$T")
      println(s"Parameters: ${"%,d".format(config.numParameters)}")

      // CPU forward pass
      val cpuStart = System.nanoTime()
      val cpuLoss = model.forwardCPU(inputs, targets, B, T)
      val cpuLogits = model.getLogits(B, T).clone()
      val cpuProbs = model.getProbs(B, T).clone()
      val cpuTime = (System.nanoTime() - cpuStart) / 1e6

      // GPU forward pass
      val gpuStart = System.nanoTime()
      val (gpuLoss, gpuLogits, gpuProbs) = GPT2Pipeline.forwardGPU(model, inputs, targets, B, T)
      val gpuTime = (System.nanoTime() - gpuStart) / 1e6

      println(f"\nCPU time: $cpuTime%.2f ms")
      println(f"GPU time: $gpuTime%.2f ms")
      println(f"Speedup: ${cpuTime / gpuTime}%.2fx")

      // Compare results - take matching lengths (CPU uses V, GPU uses Vp)
      val logitsLen = cpuLogits.length
      val logitsMaxErr = maxError(gpuLogits.take(logitsLen), cpuLogits)
      val logitsMeanErr = meanError(gpuLogits.take(logitsLen), cpuLogits)
      val probsLen = cpuProbs.length
      val probsMaxErr = maxError(gpuProbs.take(probsLen), cpuProbs)
      val probsMeanErr = meanError(gpuProbs.take(probsLen), cpuProbs)
      val lossErr = abs(gpuLoss - cpuLoss)

      println(f"\nLogits max error: $logitsMaxErr%.6f")
      println(f"Logits mean error: $logitsMeanErr%.8f")
      println(f"Probs max error: $probsMaxErr%.6f")
      println(f"Probs mean error: $probsMeanErr%.8f")
      println(f"Loss error: $lossErr%.6f (CPU=$cpuLoss%.4f, GPU=$gpuLoss%.4f)")

      // Assertions with reasonable tolerances for floating point
      assert(logitsMaxErr < 0.1f, s"Logits max error $logitsMaxErr exceeds tolerance 0.1")
      assert(probsMaxErr < 0.015f, s"Probs max error $probsMaxErr exceeds tolerance 0.015")
      assert(lossErr < 0.1f, s"Loss error $lossErr exceeds tolerance 0.1")
  }

  test("GPU forward pass matches CPU forward pass - medium model") {
    VkCyfraRuntime.using:
      // Medium model for more realistic test
      val config = GPT2Config(
        maxSeqLen = 128,
        vocabSize = 500,
        paddedVocabSize = 512,
        numLayers = 4,
        numHeads = 8,
        channels = 128,
      )

      val model = GPT2Model.random(config, seed = 42L)

      val B = 2
      val T = 32
      val rng = new scala.util.Random(123)
      val inputs = Array.fill(B * T)(rng.nextInt(config.vocabSize))
      val targets = Array.fill(B * T)(rng.nextInt(config.vocabSize))

      println(s"\n=== Medium Model Integration Test ===")
      println(s"Config: ${config.numLayers} layers, ${config.channels} channels, ${config.numHeads} heads")
      println(s"Batch: B=$B, T=$T")
      println(s"Parameters: ${"%,d".format(config.numParameters)}")

      // CPU forward pass
      val cpuStart = System.nanoTime()
      val cpuLoss = model.forwardCPU(inputs, targets, B, T)
      val cpuLogits = model.getLogits(B, T).clone()
      val cpuProbs = model.getProbs(B, T).clone()
      val cpuTime = (System.nanoTime() - cpuStart) / 1e6

      // GPU forward pass
      val gpuStart = System.nanoTime()
      val (gpuLoss, gpuLogits, gpuProbs) = GPT2Pipeline.forwardGPU(model, inputs, targets, B, T)
      val gpuTime = (System.nanoTime() - gpuStart) / 1e6

      println(f"\nCPU time: $cpuTime%.2f ms")
      println(f"GPU time: $gpuTime%.2f ms")
      println(f"Speedup: ${cpuTime / gpuTime}%.2fx")

      // Compare results
      val logitsLen = cpuLogits.length
      val logitsMaxErr = maxError(gpuLogits.take(logitsLen), cpuLogits)
      val logitsMeanErr = meanError(gpuLogits.take(logitsLen), cpuLogits)
      val probsLen = cpuProbs.length
      val probsMaxErr = maxError(gpuProbs.take(probsLen), cpuProbs)
      val lossErr = abs(gpuLoss - cpuLoss)

      println(f"\nLogits max error: $logitsMaxErr%.6f")
      println(f"Logits mean error: $logitsMeanErr%.8f")
      println(f"Probs max error: $probsMaxErr%.6f")
      println(f"Loss error: $lossErr%.6f (CPU=$cpuLoss%.4f, GPU=$gpuLoss%.4f)")

      // Assertions
      assert(logitsMaxErr < 0.5f, s"Logits max error $logitsMaxErr exceeds tolerance 0.5")
      assert(probsMaxErr < 0.05f, s"Probs max error $probsMaxErr exceeds tolerance 0.05")
      assert(lossErr < 0.5f, s"Loss error $lossErr exceeds tolerance 0.5")
  }

  test("GPU vs CPU performance comparison") {
    VkCyfraRuntime.using:
      println(s"\n=== GPU vs CPU Performance Comparison ===\n")

      // Test multiple configurations
      val configs = List(
        ("Tiny", GPT2Config(maxSeqLen = 32, vocabSize = 100, paddedVocabSize = 128, numLayers = 1, numHeads = 2, channels = 32)),
        ("Small", GPT2Config(maxSeqLen = 64, vocabSize = 100, paddedVocabSize = 128, numLayers = 2, numHeads = 4, channels = 64)),
        ("Medium", GPT2Config(maxSeqLen = 128, vocabSize = 500, paddedVocabSize = 512, numLayers = 4, numHeads = 8, channels = 128)),
      )

      val B = 2
      val warmupIterations = 2
      val benchIterations = 5

      for (name, config) <- configs do
        val T = config.maxSeqLen / 2
        val model = GPT2Model.random(config, seed = 42L)
        val rng = new scala.util.Random(123)
        val inputs = Array.fill(B * T)(rng.nextInt(config.vocabSize))
        val targets = Array.fill(B * T)(rng.nextInt(config.vocabSize))

        // Warmup
        for _ <- 0 until warmupIterations do
          model.forwardCPU(inputs, targets, B, T)
          GPT2Pipeline.forwardGPU(model, inputs, targets, B, T)

        // Benchmark CPU
        val cpuStart = System.nanoTime()
        for _ <- 0 until benchIterations do
          model.forwardCPU(inputs, targets, B, T)
        val cpuTime = (System.nanoTime() - cpuStart) / 1e6 / benchIterations

        // Benchmark GPU
        val gpuStart = System.nanoTime()
        for _ <- 0 until benchIterations do
          GPT2Pipeline.forwardGPU(model, inputs, targets, B, T)
        val gpuTime = (System.nanoTime() - gpuStart) / 1e6 / benchIterations

        val speedup = cpuTime / gpuTime
        val tokensPerSecCPU = (B * T) / (cpuTime / 1000.0)
        val tokensPerSecGPU = (B * T) / (gpuTime / 1000.0)

        println(f"$name%-8s | Params: ${config.numParameters}%,9d | B=$B T=$T%-3d")
        println(f"         | CPU: $cpuTime%8.2f ms (${tokensPerSecCPU}%6.0f tok/s)")
        println(f"         | GPU: $gpuTime%8.2f ms (${tokensPerSecGPU}%6.0f tok/s)")
        println(f"         | Speedup: ${speedup}%.2fx")
        println()
  }

  test("GPU produces valid probability distributions") {
    VkCyfraRuntime.using:
      val config = GPT2Config(
        maxSeqLen = 32,
        vocabSize = 100,
        paddedVocabSize = 128,
        numLayers = 2,
        numHeads = 4,
        channels = 64,
      )

      val model = GPT2Model.random(config, seed = 42L)
      val B = 2
      val T = 8
      val rng = new scala.util.Random(123)
      val inputs = Array.fill(B * T)(rng.nextInt(config.vocabSize))
      val targets = Array.fill(B * T)(rng.nextInt(config.vocabSize))

      val (_, _, probs) = GPT2Pipeline.forwardGPU(model, inputs, targets, B, T)

      // Verify probability properties
      for b <- 0 until B; t <- 0 until T do
        val offset = b * T * config.paddedVocabSize + t * config.paddedVocabSize
        val probSlice = probs.slice(offset, offset + config.vocabSize)
        val sum = probSlice.sum

        // All probabilities should be non-negative
        assert(probSlice.forall(_ >= 0), s"Found negative probability at ($b, $t)")

        // Sum should be close to 1
        assert(abs(sum - 1.0f) < 0.01f, s"Probability sum at ($b, $t) is $sum, expected ~1.0")

      println("GPU produces valid probability distributions ✓")
  }
