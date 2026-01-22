package io.computenode.cyfra.llm

import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import java.nio.{ByteBuffer, ByteOrder}
import scala.concurrent.duration.Duration
import scala.math.abs

/** Tests for the optimized GPU pipeline.
  *
  * Tests:
  * - Correctness: GPU matches CPU reference
  * - Performance: ByteBuffer I/O, single pipeline, no intermediate transfers
  */
class OptimizedPipelineTest extends FunSuite:

  override def munitTimeout: Duration = Duration("10 minutes")

  private def maxError(actual: Array[Float], expected: Array[Float], len: Int): Float =
    (0 until len).map(i => abs(actual(i) - expected(i))).max

  test("Optimized pipeline correctness - small model") {
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

      println(s"\n=== Optimized Pipeline Correctness Test ===")
      println(s"Config: ${config.numLayers} layers, ${config.channels} channels, ${config.numHeads} heads")
      println(s"Batch: B=$B, T=$T")

      // CPU forward pass (reference)
      val cpuLoss = model.forwardCPU(inputs, targets, B, T)
      val cpuProbs = model.getProbs(B, T).clone()

      // GPU forward pass (optimized)
      val (gpuLoss, gpuProbs) = GPT2PipelineOptimized.forwardGPUSimple(model, inputs, targets, B, T)

      // Compare
      val probsLen = cpuProbs.length
      val probsErr = maxError(gpuProbs, cpuProbs, probsLen)
      val lossErr = abs(gpuLoss - cpuLoss)

      println(f"CPU Loss: $cpuLoss%.4f")
      println(f"GPU Loss: $gpuLoss%.4f")
      println(f"Loss error: $lossErr%.6f")
      println(f"Probs max error: $probsErr%.6f")

      assert(lossErr < 1.0f, s"Loss error $lossErr exceeds tolerance 1.0")
      assert(probsErr < 0.1f, s"Probs error $probsErr exceeds tolerance 0.1")
  }

  test("Optimized pipeline performance - ByteBuffer I/O") {
    VkCyfraRuntime.using:
      println(s"\n=== Optimized Pipeline Performance ===\n")

      case class TestConfig(name: String, config: GPT2Config, B: Int, T: Int)
      val configs = List(
        TestConfig("Tiny", GPT2Config(maxSeqLen = 32, vocabSize = 100, paddedVocabSize = 128, numLayers = 1, numHeads = 2, channels = 32), 4, 16),
        TestConfig("Small", GPT2Config(maxSeqLen = 64, vocabSize = 100, paddedVocabSize = 128, numLayers = 2, numHeads = 4, channels = 64), 4, 32),
        TestConfig("Medium", GPT2Config(maxSeqLen = 128, vocabSize = 1000, paddedVocabSize = 1024, numLayers = 4, numHeads = 8, channels = 256), 4, 64),
        TestConfig("Large", GPT2Config(maxSeqLen = 256, vocabSize = 1000, paddedVocabSize = 1024, numLayers = 6, numHeads = 8, channels = 512), 2, 128),
        TestConfig("XLarge", GPT2Config(maxSeqLen = 256, vocabSize = 1000, paddedVocabSize = 1024, numLayers = 8, numHeads = 8, channels = 512), 2, 128),
      )

      for tc <- configs do
        val name = tc.name
        val config = tc.config
        val B = tc.B
        val T = tc.T
        val model = GPT2Model.random(config, seed = 42L)
        val Vp = config.paddedVocabSize

        val rng = new scala.util.Random(123)
        val inputs = Array.fill(B * T)(rng.nextInt(config.vocabSize))
        val targets = Array.fill(B * T)(rng.nextInt(config.vocabSize))

        // Pre-allocate ByteBuffers (done BEFORE timing)
        val inputBuf = GPT2PipelineOptimized.allocateIntBuffer(B * T)
        val targetBuf = GPT2PipelineOptimized.allocateIntBuffer(B * T)
        val probsBuf = GPT2PipelineOptimized.allocateBuffer(B * T * Vp)
        val lossesBuf = GPT2PipelineOptimized.allocateBuffer(B * T)

        // Copy data to buffers (part of initialization)
        GPT2PipelineOptimized.copyIntToBuffer(inputs, inputBuf)
        GPT2PipelineOptimized.copyIntToBuffer(targets, targetBuf)

        // Adjust iterations based on model size
        val cpuIters = if config.numParameters > 10_000_000 then 2 else if config.numParameters > 1_000_000 then 3 else 10
        val gpuIters = if config.numParameters > 10_000_000 then 2 else 3

        // Warmup
        for _ <- 0 until 1 do
          model.forwardCPU(inputs, targets, B, T)
          GPT2PipelineOptimized.forwardGPU(model, inputBuf, targetBuf, B, T, probsBuf, lossesBuf)

        // Benchmark CPU
        val cpuStart = System.nanoTime()
        for _ <- 0 until cpuIters do
          model.forwardCPU(inputs, targets, B, T)
        val cpuTime = (System.nanoTime() - cpuStart) / 1e6 / cpuIters

        // Benchmark GPU (with ByteBuffer - no Array iteration)
        val gpuStart = System.nanoTime()
        for _ <- 0 until gpuIters do
          GPT2PipelineOptimized.forwardGPU(model, inputBuf, targetBuf, B, T, probsBuf, lossesBuf)
        val gpuTime = (System.nanoTime() - gpuStart) / 1e6 / gpuIters

        val speedup = cpuTime / gpuTime
        val tokensPerSecCPU = (B * T) / (cpuTime / 1000.0)
        val tokensPerSecGPU = (B * T) / (gpuTime / 1000.0)

        println(f"$name%-8s | Params: ${config.numParameters}%,9d | B=$B T=$T%-3d")
        println(f"         | CPU: $cpuTime%8.2f ms (${tokensPerSecCPU}%6.0f tok/s)")
        println(f"         | GPU: $gpuTime%8.2f ms (${tokensPerSecGPU}%6.0f tok/s)")
        println(f"         | Speedup: ${speedup}%.2fx")
        println()
  }

  test("Compare old vs optimized pipeline performance") {
    VkCyfraRuntime.using:
      println(s"\n=== Old vs Optimized Pipeline Comparison ===\n")

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

      // Pre-allocate ByteBuffers for optimized
      val inputBuf = GPT2PipelineOptimized.allocateIntBuffer(B * T)
      val targetBuf = GPT2PipelineOptimized.allocateIntBuffer(B * T)
      val probsBuf = GPT2PipelineOptimized.allocateBuffer(B * T * config.paddedVocabSize)
      val lossesBuf = GPT2PipelineOptimized.allocateBuffer(B * T)

      GPT2PipelineOptimized.copyIntToBuffer(inputs, inputBuf)
      GPT2PipelineOptimized.copyIntToBuffer(targets, targetBuf)

      // Warmup
      model.forwardCPU(inputs, targets, B, T)
      GPT2Pipeline.forwardGPU(model, inputs, targets, B, T)
      GPT2PipelineOptimized.forwardGPU(model, inputBuf, targetBuf, B, T, probsBuf, lossesBuf)

      val iters = 5

      // Benchmark old GPU pipeline (with Array iteration and CPU attention)
      val oldGpuStart = System.nanoTime()
      for _ <- 0 until iters do
        GPT2Pipeline.forwardGPU(model, inputs, targets, B, T)
      val oldGpuTime = (System.nanoTime() - oldGpuStart) / 1e6 / iters

      // Benchmark optimized GPU pipeline (ByteBuffer, single execution, GPU attention)
      val optGpuStart = System.nanoTime()
      for _ <- 0 until iters do
        GPT2PipelineOptimized.forwardGPU(model, inputBuf, targetBuf, B, T, probsBuf, lossesBuf)
      val optGpuTime = (System.nanoTime() - optGpuStart) / 1e6 / iters

      // Benchmark CPU
      val cpuStart = System.nanoTime()
      for _ <- 0 until iters do
        model.forwardCPU(inputs, targets, B, T)
      val cpuTime = (System.nanoTime() - cpuStart) / 1e6 / iters

      println(f"CPU:              $cpuTime%8.2f ms")
      println(f"Old GPU Pipeline: $oldGpuTime%8.2f ms (${cpuTime / oldGpuTime}%.2fx vs CPU)")
      println(f"Optimized GPU:    $optGpuTime%8.2f ms (${cpuTime / optGpuTime}%.2fx vs CPU)")
      println(f"Optimization speedup: ${oldGpuTime / optGpuTime}%.2fx")
  }
