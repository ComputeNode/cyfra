package io.computenode.cyfra.llm

import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llm.programs.*
import io.computenode.cyfra.llm.programs.GeLUProgram.{GeLUForwardLayout, GeLUSizes}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import scala.concurrent.duration.Duration

/** Performance tests for cyfra-llm GPU programs.
  *
  * These tests measure the performance of various GPU operations.
  */
class PerformanceTest extends FunSuite:

  override def munitTimeout: Duration = Duration("5 minutes")

  test("CPU forward pass performance - small model") {
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

      val B = 4
      val T = 32
      val rng = new scala.util.Random(123)
      val inputs = Array.fill(B * T)(rng.nextInt(config.vocabSize))
      val targets = Array.fill(B * T)(rng.nextInt(config.vocabSize))

      // Warmup
      model.forwardCPU(inputs, targets, B, T)

      // Benchmark
      val numIterations = 10
      val startTime = System.nanoTime()
      for _ <- 0 until numIterations do
        model.forwardCPU(inputs, targets, B, T)
      val endTime = System.nanoTime()

      val totalTimeMs = (endTime - startTime) / 1e6
      val avgTimeMs = totalTimeMs / numIterations
      val tokensPerSecond = (B * T * numIterations) / (totalTimeMs / 1000.0)

      println(s"\nCPU Forward Pass Performance (small model):")
      println(s"  Config: ${config.numLayers} layers, ${config.channels} channels, ${config.numHeads} heads")
      println(s"  Batch: B=$B, T=$T")
      println(f"  Total time for $numIterations iterations: $totalTimeMs%.2f ms")
      println(f"  Average time per forward: $avgTimeMs%.2f ms")
      println(f"  Tokens/second: $tokensPerSecond%.0f")
  }

  test("CPU forward pass performance - medium model") {
    VkCyfraRuntime.using:
      val config = GPT2Config(
        maxSeqLen = 256,
        vocabSize = 1000,
        paddedVocabSize = 1024,
        numLayers = 6,
        numHeads = 8,
        channels = 256,
      )

      val model = GPT2Model.random(config, seed = 42L)

      val B = 2
      val T = 64
      val rng = new scala.util.Random(123)
      val inputs = Array.fill(B * T)(rng.nextInt(config.vocabSize))
      val targets = Array.fill(B * T)(rng.nextInt(config.vocabSize))

      // Warmup
      model.forwardCPU(inputs, targets, B, T)

      // Benchmark
      val numIterations = 5
      val startTime = System.nanoTime()
      for _ <- 0 until numIterations do
        model.forwardCPU(inputs, targets, B, T)
      val endTime = System.nanoTime()

      val totalTimeMs = (endTime - startTime) / 1e6
      val avgTimeMs = totalTimeMs / numIterations
      val tokensPerSecond = (B * T * numIterations) / (totalTimeMs / 1000.0)

      println(s"\nCPU Forward Pass Performance (medium model):")
      println(s"  Config: ${config.numLayers} layers, ${config.channels} channels, ${config.numHeads} heads")
      println(s"  Parameters: ${"%,d".format(config.numParameters)}")
      println(s"  Batch: B=$B, T=$T")
      println(f"  Total time for $numIterations iterations: $totalTimeMs%.2f ms")
      println(f"  Average time per forward: $avgTimeMs%.2f ms")
      println(f"  Tokens/second: $tokensPerSecond%.0f")
  }

  test("GPU GELU program performance") {
    VkCyfraRuntime.using:
      val N = 1024 * 1024 // 1M elements

      // Prepare input data
      val rng = new scala.util.Random(42)
      val inputData = Array.fill(N)((rng.nextFloat() - 0.5f) * 4.0f)
      val outputData = new Array[Float](N)

      val sizes = GeLUSizes(N)
      val region = GBufferRegion
        .allocate[GeLUForwardLayout]
        .map: layout =>
          GeLUProgram.forward(sizes).execute(sizes, layout)

      // Warmup
      region.runUnsafe(
        init = GeLUForwardLayout(
          out = GBuffer(outputData),
          inp = GBuffer(inputData),
          params = GUniform(GPT2Params.forGelu(N)),
        ),
        onDone = layout => layout.out.readArray(outputData),
      )

      // Benchmark
      val numIterations = 100
      val startTime = System.nanoTime()
      for _ <- 0 until numIterations do
        region.runUnsafe(
          init = GeLUForwardLayout(
            out = GBuffer(outputData),
            inp = GBuffer(inputData),
            params = GUniform(GPT2Params.forGelu(N)),
          ),
          onDone = layout => layout.out.readArray(outputData),
        )
      val endTime = System.nanoTime()

      val totalTimeMs = (endTime - startTime) / 1e6
      val avgTimeMs = totalTimeMs / numIterations
      val elementsPerSecond = (N.toDouble * numIterations) / (totalTimeMs / 1000.0)
      val gbPerSecond = (N.toDouble * 4 * 2 * numIterations) / (totalTimeMs / 1000.0) / 1e9 // read + write

      val throughputBillion = elementsPerSecond / 1e9
      println(s"\nGPU GELU Performance:")
      println(s"  Elements: ${"%,d".format(N)}")
      println(s"  Iterations: $numIterations")
      println(f"  Total time: $totalTimeMs%.2f ms")
      println(f"  Average time: $avgTimeMs%.3f ms")
      println(f"  Throughput: $throughputBillion%.2f billion elements/sec")
      println(f"  Memory bandwidth: $gbPerSecond%.2f GB/s")

      // Verify correctness
      val cpuGelu = (x: Float) => {
        val scalingFactor = math.sqrt(2.0 / math.Pi).toFloat
        val cube = 0.044715f * x * x * x
        0.5f * x * (1.0f + math.tanh(scalingFactor * (x + cube)).toFloat)
      }
      val maxError = inputData.zip(outputData).map { case (in, out) =>
        math.abs(cpuGelu(in) - out)
      }.max
      println(s"  Max error vs CPU: $maxError")
      assert(maxError < 0.001f, s"Max error too large: $maxError")
  }

  test("GPU MatMul-like dot product performance") {
    VkCyfraRuntime.using:
      val BT = 4096
      val C = 256

      case class DotProductLayout(
        inp: GBuffer[Float32],
        weight: GBuffer[Float32],
        out: GBuffer[Float32],
        params: GUniform[GPT2Params],
      ) derives Layout

      val dotProgram: GProgram[Int, DotProductLayout] =
        GProgram[Int, DotProductLayout](
          layout = bt =>
            DotProductLayout(
              inp = GBuffer[Float32](bt * C),
              weight = GBuffer[Float32](C),
              out = GBuffer[Float32](bt),
              params = GUniform[GPT2Params](),
            ),
          dispatch = (_, bt) => StaticDispatch(((bt + 255) / 256, 1, 1)),
          workgroupSize = (256, 1, 1),
        ): layout =>
          val idx = GIO.invocationId
          val params = layout.params.read
          val totalBT = params.B  // BT stored in B field
          val channels = params.C

          GIO.when(idx < totalBT):
            // Compute dot product of row idx with weight vector
            val dotProduct = GSeq
              .gen[Int32](0, _ + 1)
              .limit(256)
              .fold(0.0f, (sum: Float32, i: Int32) => {
                when(i < channels):
                  val inpVal = GIO.read[Float32](layout.inp, idx * channels + i)
                  val weightVal = GIO.read[Float32](layout.weight, i)
                  sum + inpVal * weightVal
                .otherwise(sum)
              })
            GIO.write(layout.out, idx, dotProduct)

      val rng = new scala.util.Random(42)
      val inputData = Array.fill(BT * C)(rng.nextFloat())
      val weightData = Array.fill(C)(rng.nextFloat())
      val outputData = new Array[Float](BT)

      val region = GBufferRegion
        .allocate[DotProductLayout]
        .map: layout =>
          dotProgram.execute(BT, layout)

      // Warmup
      region.runUnsafe(
        init = DotProductLayout(
          inp = GBuffer(inputData),
          weight = GBuffer(weightData),
          out = GBuffer(outputData),
          params = GUniform(GPT2Params.forMatmul(BT, C, 1, false)),
        ),
        onDone = layout => layout.out.readArray(outputData),
      )

      // Benchmark
      val numIterations = 50
      val startTime = System.nanoTime()
      for _ <- 0 until numIterations do
        region.runUnsafe(
          init = DotProductLayout(
            inp = GBuffer(inputData),
            weight = GBuffer(weightData),
            out = GBuffer(outputData),
            params = GUniform(GPT2Params.forMatmul(BT, C, 1, false)),
          ),
          onDone = layout => layout.out.readArray(outputData),
        )
      val endTime = System.nanoTime()

      val totalTimeMs = (endTime - startTime) / 1e6
      val avgTimeMs = totalTimeMs / numIterations
      val flops = BT.toDouble * C * 2 * numIterations // multiply-add per element
      val gflops = flops / (totalTimeMs / 1000.0) / 1e9

      val totalFlopsPerIter = BT * C * 2
      println(s"\nGPU Dot Product Performance:")
      println(s"  BT: $BT, C: $C")
      println(s"  Total FLOPs per iteration: ${"%,d".format(totalFlopsPerIter)}")
      println(s"  Iterations: $numIterations")
      println(f"  Total time: $totalTimeMs%.2f ms")
      println(f"  Average time: $avgTimeMs%.3f ms")
      println(f"  Performance: $gflops%.2f GFLOPS")

      // Verify correctness
      val cpuOutput = (0 until BT).map { bt =>
        (0 until C).map(i => inputData(bt * C + i) * weightData(i)).sum
      }.toArray
      val maxError = cpuOutput.zip(outputData).map { case (cpu, gpu) =>
        math.abs(cpu - gpu)
      }.max
      println(s"  Max error vs CPU: $maxError")
      assert(maxError < 0.01f, s"Max error too large: $maxError")
  }
