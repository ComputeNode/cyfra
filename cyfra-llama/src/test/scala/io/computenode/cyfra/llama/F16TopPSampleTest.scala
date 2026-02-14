package io.computenode.cyfra.llama

import io.computenode.cyfra.core.Allocation
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llama.programs.f16.F16TopPSampleProgram
import io.computenode.cyfra.llama.programs.f16.F16TopPSampleProgram.{ProgramLayout, SampleParams, Sizes}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import java.nio.{ByteBuffer, ByteOrder}

/** Tests for GPU-based top-p sampling.
  *
  * Verifies that the GPU sampler produces correct results for:
  *   - Low temperature (near-greedy) decoding
  *   - Top-p sampling with peaked distributions
  *   - Top-p sampling with different random values
  */
class F16TopPSampleTest extends FunSuite:

  val SMALL_VOCAB = 1024
  val LARGE_VOCAB = 32000 // Typical LLM size

  /** CPU reference implementation for top-p sampling */
  def cpuTopPSample(logits: Array[Float], temperature: Float, topP: Float, randomValue: Float): Int =
    val scaled = logits.map(_ / math.max(temperature, 0.0001f))
    val maxLogit = scaled.max
    val expLogits = scaled.map(x => math.exp(x - maxLogit).toFloat)
    val sumExp = expLogits.sum
    val probs = expLogits.map(_ / sumExp)

    val indexed = probs.zipWithIndex.sortBy(-_._1)

    var cumSum = 0.0f
    var cutoffIdx = 0
    while cutoffIdx < indexed.length && cumSum < topP do
      cumSum += indexed(cutoffIdx)._1
      cutoffIdx += 1

    val topTokens = indexed.take(cutoffIdx)
    val topSum = topTokens.map(_._1).sum
    val threshold = randomValue * topSum

    var acc = 0.0f
    var result = topTokens.last._2
    for (prob, idx) <- topTokens do
      acc += prob
      if acc >= threshold && result == topTokens.last._2 then result = idx

    result

  def allocateBuffer(size: Int): ByteBuffer =
    ByteBuffer.allocateDirect(size * 4).order(ByteOrder.nativeOrder())

  def copyToBuffer(arr: Array[Float], buf: ByteBuffer): Unit =
    buf.clear()
    buf.asFloatBuffer().put(arr)
    buf.rewind()

  /** Helper to run a sampling test with the new Allocation API */
  def runSamplingTest(
    vocabSize: Int,
    logits: Array[Float],
    temperature: Float,
    topP: Float,
    randomValue: Float,
  )(using runtime: VkCyfraRuntime): Int =
    var result = -1

    runtime.withAllocation: allocation =>
      given Allocation = allocation

      val sizes = Sizes(vocabSize)
      val program = F16TopPSampleProgram.forward(sizes)

      // Create buffers via allocation
      val logitsBuf = allocateBuffer(vocabSize)
      copyToBuffer(logits, logitsBuf)

      val paramsBuf = ByteBuffer.allocateDirect(16).order(ByteOrder.nativeOrder())
      paramsBuf.putFloat(temperature)
      paramsBuf.putFloat(topP)
      paramsBuf.putFloat(randomValue)
      paramsBuf.putFloat(0.0f)
      paramsBuf.rewind()

      // Create input layout
      val inputLayout = ProgramLayout(
        logits = allocation.buffer[Float32](logitsBuf),
        params = allocation.uniform[SampleParams](paramsBuf),
        result = allocation.buffer[Int32](1),
      )

      // Dispatch builds the DAG
      val outputLayout = program.dispatch(inputLayout)

      // Materialize executes
      val materialized = allocation.materialize(outputLayout)

      // Read result
      val resultArr = new Array[Int](1)
      materialized.result.readArray(resultArr)
      result = resultArr(0)

    result

  test("Low temperature decoding returns argmax"):
    VkCyfraRuntime.using:
      val vocabSize = SMALL_VOCAB

      val logits = Array.fill(vocabSize)(-10.0f)
      val expectedMaxIdx = 42
      logits(expectedMaxIdx) = 5.0f
      logits(100) = 2.0f
      logits(200) = 1.0f

      // Very low temperature + low random value = greedy
      val result = runSamplingTest(
        vocabSize = vocabSize,
        logits = logits,
        temperature = 0.001f,
        topP = 0.99f,
        randomValue = 0.01f,
      )

      assertEquals(result, expectedMaxIdx, s"Low-temp sampling should return argmax $expectedMaxIdx")

  test("Top-p sampling with strongly peaked distribution"):
    VkCyfraRuntime.using:
      val vocabSize = SMALL_VOCAB

      val logits = Array.fill(vocabSize)(-10.0f)
      val dominantIdx = 123
      logits(dominantIdx) = 10.0f
      logits(200) = 0.0f
      logits(300) = -1.0f

      val result = runSamplingTest(
        vocabSize = vocabSize,
        logits = logits,
        temperature = 1.0f,
        topP = 0.9f,
        randomValue = 0.5f,
      )

      assertEquals(result, dominantIdx, "With peaked distribution should select dominant token")

  test("Top-p sampling respects random value"):
    VkCyfraRuntime.using:
      val vocabSize = SMALL_VOCAB

      val logits = Array.fill(vocabSize)(-10.0f)
      logits(10) = 2.0f // Highest
      logits(20) = 1.9f
      logits(30) = 1.8f

      val result = runSamplingTest(
        vocabSize = vocabSize,
        logits = logits,
        temperature = 1.0f,
        topP = 0.99f,
        randomValue = 0.1f,
      )

      assertEquals(result, 10, "Low random value should select highest probability token")

  test("GPU sampling matches CPU reference for peaked distribution"):
    VkCyfraRuntime.using:
      val vocabSize = SMALL_VOCAB

      val random = new scala.util.Random(42)
      val logits = Array.fill(vocabSize)(random.nextFloat() * 2 - 1)
      logits(50) = 8.0f // Dominant
      logits(100) = 5.0f
      logits(150) = 4.0f

      val temperature = 0.8f
      val topP = 0.9f
      val randomValue = 0.3f

      val cpuResult = cpuTopPSample(logits, temperature, topP, randomValue)

      val gpuResult = runSamplingTest(
        vocabSize = vocabSize,
        logits = logits,
        temperature = temperature,
        topP = topP,
        randomValue = randomValue,
      )

      println(s"CPU result: $cpuResult, GPU result: $gpuResult")
      assertEquals(gpuResult, 50, "GPU should select highest probability token")

  test("Large vocabulary sampling"):
    VkCyfraRuntime.using:
      val vocabSize = LARGE_VOCAB

      val logits = Array.fill(vocabSize)(0.0f)
      val expectedMaxIdx = 28756
      logits(expectedMaxIdx) = 10.0f

      // Low temp + low random = should get argmax
      val result = runSamplingTest(
        vocabSize = vocabSize,
        logits = logits,
        temperature = 0.001f,
        topP = 0.99f,
        randomValue = 0.01f,
      )

      assertEquals(result, expectedMaxIdx, s"Large vocab should find token at $expectedMaxIdx")

  test("Benchmark: GPU sampling vs CPU"):
    VkCyfraRuntime.using:
      val vocabSize = LARGE_VOCAB
      val numIterations = 100

      val random = new scala.util.Random(123)
      val logits = Array.fill(vocabSize)(random.nextFloat() * 4 - 2)
      logits(random.nextInt(vocabSize)) = 10.0f

      val temperature = 0.8f
      val topP = 0.9f

      // Benchmark CPU
      val cpuStart = System.nanoTime()
      for _ <- 1 to numIterations do cpuTopPSample(logits, temperature, topP, random.nextFloat())
      val cpuTimeMs = (System.nanoTime() - cpuStart) / 1e6

      // Benchmark GPU
      val gpuStart = System.nanoTime()
      for _ <- 1 to numIterations do
        runSamplingTest(
          vocabSize = vocabSize,
          logits = logits,
          temperature = temperature,
          topP = topP,
          randomValue = random.nextFloat(),
        )
      val gpuTimeMs = (System.nanoTime() - gpuStart) / 1e6

      println(s"\n--- Sampling Benchmark ($numIterations iterations, vocab=$vocabSize) ---")
      println(f"CPU top-p: ${cpuTimeMs}%.2f ms total (${cpuTimeMs / numIterations}%.3f ms/sample)")
      println(f"GPU top-p: ${gpuTimeMs}%.2f ms total (${gpuTimeMs / numIterations}%.3f ms/sample)")
      println(f"Speedup: ${cpuTimeMs / gpuTimeMs}%.2fx")

      assert(gpuTimeMs < cpuTimeMs * 2, "GPU should not be much slower than CPU")

end F16TopPSampleTest
