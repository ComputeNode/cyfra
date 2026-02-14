package io.computenode.cyfra.llama

import io.computenode.cyfra.core.{Allocation, GCodec}
import io.computenode.cyfra.core.GCodec.given
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.llama.programs.f16.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Random

class F16ProgramsTest extends FunSuite:
  var runtime: VkCyfraRuntime = null

  override def beforeAll(): Unit =
    runtime = VkCyfraRuntime()

  override def afterAll(): Unit =
    if runtime != null then runtime.close()

  private def allocateF16Buffer(size: Int): ByteBuffer =
    ByteBuffer.allocateDirect(size * 2).order(ByteOrder.nativeOrder())

  private def allocateF32Buffer(size: Int): ByteBuffer =
    ByteBuffer.allocateDirect(size * 4).order(ByteOrder.nativeOrder())

  private def allocateIntBuffer(size: Int): ByteBuffer =
    ByteBuffer.allocateDirect(size * 4).order(ByteOrder.nativeOrder())

  private def fillF16Random(buf: ByteBuffer, scale: Float = 0.1f): Unit =
    val shortBuf = buf.asShortBuffer()
    val rand = new Random(42)
    while shortBuf.hasRemaining do
      val f = (rand.nextFloat() * 2 - 1) * scale
      shortBuf.put(java.lang.Float.floatToFloat16(f))
    buf.rewind()

  private def readF16(buf: ByteBuffer, index: Int): Float =
    java.lang.Float.float16ToFloat(buf.asShortBuffer().get(index))

  private def readF32(buf: ByteBuffer, index: Int): Float =
    buf.asFloatBuffer().get(index)

  // Test 1: Simple buffer write and read
  test("buffer write and read works"):
    runtime.withAllocation: allocation =>
      given Allocation = allocation

      val buf = allocateIntBuffer(4)
      buf.asIntBuffer().put(Array(42, 100, 200, 300))
      buf.rewind()

      val gpuBuf = allocation.buffer[Int32](buf)
      
      val readBuf = allocateIntBuffer(4)
      gpuBuf.readTo(readBuf)
      readBuf.rewind()
      
      assertEquals(readBuf.asIntBuffer().get(0), 42)
      assertEquals(readBuf.asIntBuffer().get(1), 100)
      assertEquals(readBuf.asIntBuffer().get(2), 200)
      assertEquals(readBuf.asIntBuffer().get(3), 300)

  // Test 2: F16 Embedding program alone
  test("F16EmbeddingProgram produces correct output"):
    runtime.withAllocation: allocation =>
      given Allocation = allocation

      val vocabSize = 1000
      val hiddenSize = 64
      val seqLen = 2

      // Create token buffer with tokens [5, 10]
      val tokenBuf = allocateIntBuffer(seqLen)
      tokenBuf.asIntBuffer().put(Array(5, 10))
      tokenBuf.rewind()

      // Create embedding table
      val embedBuf = allocateF16Buffer(vocabSize * hiddenSize)
      val embedShort = embedBuf.asShortBuffer()
      for v <- 0 until vocabSize do
        for h <- 0 until hiddenSize do
          // Each token v has embedding [v*0.01, v*0.01+0.001, v*0.01+0.002, ...]
          val value = v * 0.01f + h * 0.001f
          embedShort.put(v * hiddenSize + h, java.lang.Float.floatToFloat16(value))

      // Create output buffer
      val outputBuf = allocateF16Buffer(seqLen * hiddenSize)

      // Create GPU buffers
      val tokens = allocation.buffer[Int32](tokenBuf)
      val embeddings = allocation.buffer[Float16](embedBuf)
      val output = allocation.buffer[Float16](outputBuf)

      // Run program
      val sizes = F16EmbeddingProgram.Sizes(seqLen, hiddenSize, vocabSize)
      val inputLayout = F16EmbeddingProgram.ProgramLayout(tokens, embeddings, output)
      val outputLayout = F16EmbeddingProgram.forward(sizes).dispatch(inputLayout)

      // Materialize
      val materialized = allocation.materialize(outputLayout)

      // Read output
      val resultBuf = allocateF16Buffer(seqLen * hiddenSize)
      materialized.output.readTo(resultBuf)
      resultBuf.rewind()

      // Token 5 should have embedding [0.05, 0.051, 0.052, ...]
      val embed5_0 = readF16(resultBuf, 0)
      val embed5_1 = readF16(resultBuf, 1)
      assertEqualsFloat(embed5_0, 0.05f, 0.001f)
      assertEqualsFloat(embed5_1, 0.051f, 0.001f)

      // Token 10 should have embedding [0.1, 0.101, 0.102, ...]
      val embed10_0 = readF16(resultBuf, hiddenSize)
      val embed10_1 = readF16(resultBuf, hiddenSize + 1)
      assertEqualsFloat(embed10_0, 0.1f, 0.001f)
      assertEqualsFloat(embed10_1, 0.101f, 0.001f)

  // Test 3: RMSNorm program alone
  test("F16RMSNormProgram produces correct output"):
    runtime.withAllocation: allocation =>
      given Allocation = allocation

      val batchSize = 1
      val hiddenSize = 64
      val eps = 1e-5f

      // Create input: [1, 2, 3, 4, ...] 
      val inputBuf = allocateF16Buffer(batchSize * hiddenSize)
      val inputShort = inputBuf.asShortBuffer()
      for i <- 0 until hiddenSize do
        inputShort.put(i, java.lang.Float.floatToFloat16((i + 1).toFloat))

      // Create weight: all 1s
      val weightBuf = allocateF16Buffer(hiddenSize)
      val weightShort = weightBuf.asShortBuffer()
      for i <- 0 until hiddenSize do
        weightShort.put(i, java.lang.Float.floatToFloat16(1.0f))

      val outputBuf = allocateF16Buffer(batchSize * hiddenSize)

      val input = allocation.buffer[Float16](inputBuf)
      val weight = allocation.buffer[Float16](weightBuf)
      val output = allocation.buffer[Float16](outputBuf)

      val sizes = F16RMSNormProgram.Sizes(batchSize, hiddenSize, eps, 0, hiddenSize)
      val inputLayout = F16RMSNormProgram.ProgramLayout(input, weight, output)
      val outputLayout = F16RMSNormProgram.forward(sizes).dispatch(inputLayout)

      val materialized = allocation.materialize(outputLayout)

      val resultBuf = allocateF16Buffer(batchSize * hiddenSize)
      materialized.output.readTo(resultBuf)
      resultBuf.rewind()

      // Compute expected RMS
      var sumSq = 0.0
      for i <- 0 until hiddenSize do
        val v = (i + 1).toDouble
        sumSq += v * v
      val rms = Math.sqrt(sumSq / hiddenSize + eps)

      // First element should be 1 / rms
      val expected0 = (1.0 / rms).toFloat
      val actual0 = readF16(resultBuf, 0)
      println(s"RMSNorm: expected=$expected0, actual=$actual0")
      assertEqualsFloat(actual0, expected0, 0.01f)

  // Test 4: Two programs chained - Embedding then RMSNorm
  test("Embedding + RMSNorm chained"):
    runtime.withAllocation: allocation =>
      given Allocation = allocation

      val vocabSize = 1000
      val hiddenSize = 64
      val seqLen = 1
      val eps = 1e-5f

      // Token buffer
      val tokenBuf = allocateIntBuffer(seqLen)
      tokenBuf.asIntBuffer().put(Array(5))
      tokenBuf.rewind()

      // Embedding table - token 5 has [0.05, 0.051, 0.052, ...]
      val embedBuf = allocateF16Buffer(vocabSize * hiddenSize)
      val embedShort = embedBuf.asShortBuffer()
      for v <- 0 until vocabSize do
        for h <- 0 until hiddenSize do
          val value = v * 0.01f + h * 0.001f
          embedShort.put(v * hiddenSize + h, java.lang.Float.floatToFloat16(value))

      // RMSNorm weight - all 1s
      val weightBuf = allocateF16Buffer(hiddenSize)
      val weightShort = weightBuf.asShortBuffer()
      for i <- 0 until hiddenSize do
        weightShort.put(i, java.lang.Float.floatToFloat16(1.0f))

      // Intermediate and output buffers
      val hiddenBuf = allocateF16Buffer(seqLen * hiddenSize)
      val normOutBuf = allocateF16Buffer(seqLen * hiddenSize)

      val tokens = allocation.buffer[Int32](tokenBuf)
      val embeddings = allocation.buffer[Float16](embedBuf)
      var hidden = allocation.buffer[Float16](hiddenBuf)
      val weight = allocation.buffer[Float16](weightBuf)
      var normOut = allocation.buffer[Float16](normOutBuf)

      // Run embedding
      val embSizes = F16EmbeddingProgram.Sizes(seqLen, hiddenSize, vocabSize)
      val embLayout = F16EmbeddingProgram.ProgramLayout(tokens, embeddings, hidden)
      val afterEmb = F16EmbeddingProgram.forward(embSizes).dispatch(embLayout)
      hidden = afterEmb.output

      // Run RMSNorm on embedding output (NO intermediate materialization)
      val normSizes = F16RMSNormProgram.Sizes(seqLen, hiddenSize, eps, 0, hiddenSize)
      val normLayout = F16RMSNormProgram.ProgramLayout(hidden, weight, normOut)
      val afterNorm = F16RMSNormProgram.forward(normSizes).dispatch(normLayout)
      normOut = afterNorm.output
      

      // Materialize only the final output
      case class FinalLayout(output: GBuffer[Float16]) derives Layout
      val materialized = allocation.materialize(FinalLayout(normOut))

      val resultBuf = allocateF16Buffer(seqLen * hiddenSize)
      materialized.output.readTo(resultBuf)
      resultBuf.rewind()

      // Token 5 embedding is [0.05, 0.051, 0.052, ...]
      // Compute expected RMS of this embedding
      var sumSq = 0.0
      for i <- 0 until hiddenSize do
        val v = 0.05 + i * 0.001
        sumSq += v * v
      val rms = Math.sqrt(sumSq / hiddenSize + eps)

      // First element: 0.05 / rms
      val expected0 = (0.05 / rms).toFloat
      val actual0 = readF16(resultBuf, 0)
      println(s"Chained Emb+Norm: expected=$expected0, actual=$actual0")
      assertEqualsFloat(actual0, expected0, 0.01f)

  // Test 5: Token changes between runs
  test("Different tokens produce different outputs"):
    runtime.withAllocation: allocation =>
      given Allocation = allocation

      val vocabSize = 1000
      val hiddenSize = 64
      val seqLen = 1

      // Embedding table
      val embedBuf = allocateF16Buffer(vocabSize * hiddenSize)
      val embedShort = embedBuf.asShortBuffer()
      for v <- 0 until vocabSize do
        for h <- 0 until hiddenSize do
          val value = v * 0.01f + h * 0.001f
          embedShort.put(v * hiddenSize + h, java.lang.Float.floatToFloat16(value))

      val embeddings = allocation.buffer[Float16](embedBuf)
      val outputBuf = allocateF16Buffer(seqLen * hiddenSize)
      val output = allocation.buffer[Float16](outputBuf)

      // First token: 5
      val tokenBuf1 = allocateIntBuffer(seqLen)
      tokenBuf1.asIntBuffer().put(Array(5))
      tokenBuf1.rewind()
      val tokens1 = allocation.buffer[Int32](tokenBuf1)

      val sizes = F16EmbeddingProgram.Sizes(seqLen, hiddenSize, vocabSize)
      val layout1 = F16EmbeddingProgram.ProgramLayout(tokens1, embeddings, output)
      val after1 = F16EmbeddingProgram.forward(sizes).dispatch(layout1)

      case class OutputLayout(output: GBuffer[Float16]) derives Layout
      val mat1 = allocation.materialize(OutputLayout(after1.output))

      val resultBuf1 = allocateF16Buffer(seqLen * hiddenSize)
      mat1.output.readTo(resultBuf1)
      resultBuf1.rewind()
      val value1 = readF16(resultBuf1, 0)

      // Second token: 10 (write to SAME buffer, different GPU buffer)
      val tokenBuf2 = allocateIntBuffer(seqLen)
      tokenBuf2.asIntBuffer().put(Array(10))
      tokenBuf2.rewind()
      val tokens2 = allocation.buffer[Int32](tokenBuf2)

      // IMPORTANT: Create a new output buffer to avoid reading stale data
      val outputBuf2 = allocateF16Buffer(seqLen * hiddenSize)
      val output2 = allocation.buffer[Float16](outputBuf2)

      val layout2 = F16EmbeddingProgram.ProgramLayout(tokens2, embeddings, output2)
      val after2 = F16EmbeddingProgram.forward(sizes).dispatch(layout2)
      val mat2 = allocation.materialize(OutputLayout(after2.output))

      val resultBuf2 = allocateF16Buffer(seqLen * hiddenSize)
      mat2.output.readTo(resultBuf2)
      resultBuf2.rewind()
      val value2 = readF16(resultBuf2, 0)

      println(s"Token 5 -> ${value1}, Token 10 -> ${value2}")

      // Token 5 should give 0.05, token 10 should give 0.1
      assertEqualsFloat(value1, 0.05f, 0.001f)
      assertEqualsFloat(value2, 0.1f, 0.001f)
      assertNotEquals(value1, value2)

  // Test 6: Token REWRITE in same buffer between runs
  test("Rewriting token buffer produces different outputs"):
    runtime.withAllocation: allocation =>
      given Allocation = allocation

      val vocabSize = 1000
      val hiddenSize = 64
      val seqLen = 1

      // Embedding table
      val embedBuf = allocateF16Buffer(vocabSize * hiddenSize)
      val embedShort = embedBuf.asShortBuffer()
      for v <- 0 until vocabSize do
        for h <- 0 until hiddenSize do
          val value = v * 0.01f + h * 0.001f
          embedShort.put(v * hiddenSize + h, java.lang.Float.floatToFloat16(value))

      val embeddings = allocation.buffer[Float16](embedBuf)

      // Create ONE token buffer - we'll rewrite it
      val tokenBuf = allocateIntBuffer(seqLen)
      val tokens = allocation.buffer[Int32](tokenBuf)

      val sizes = F16EmbeddingProgram.Sizes(seqLen, hiddenSize, vocabSize)
      case class OutputLayout(output: GBuffer[Float16]) derives Layout

      // First run: token 5
      tokenBuf.clear()
      tokenBuf.asIntBuffer().put(Array(5))
      tokenBuf.rewind()
      tokens.writeFrom(tokenBuf)

      val outputBuf1 = allocateF16Buffer(seqLen * hiddenSize)
      val output1 = allocation.buffer[Float16](outputBuf1)
      val layout1 = F16EmbeddingProgram.ProgramLayout(tokens, embeddings, output1)
      val after1 = F16EmbeddingProgram.forward(sizes).dispatch(layout1)
      val mat1 = allocation.materialize(OutputLayout(after1.output))

      val resultBuf1 = allocateF16Buffer(seqLen * hiddenSize)
      mat1.output.readTo(resultBuf1)
      resultBuf1.rewind()
      val value1 = readF16(resultBuf1, 0)

      // Second run: token 10 (REWRITE same GPU buffer)
      tokenBuf.clear()
      tokenBuf.asIntBuffer().put(Array(10))
      tokenBuf.rewind()
      tokens.writeFrom(tokenBuf)

      val outputBuf2 = allocateF16Buffer(seqLen * hiddenSize)
      val output2 = allocation.buffer[Float16](outputBuf2)
      val layout2 = F16EmbeddingProgram.ProgramLayout(tokens, embeddings, output2)
      val after2 = F16EmbeddingProgram.forward(sizes).dispatch(layout2)
      val mat2 = allocation.materialize(OutputLayout(after2.output))

      val resultBuf2 = allocateF16Buffer(seqLen * hiddenSize)
      mat2.output.readTo(resultBuf2)
      resultBuf2.rewind()
      val value2 = readF16(resultBuf2, 0)

      println(s"Rewrite: Token 5 -> ${value1}, Token 10 -> ${value2}")

      // Should be different!
      assertEqualsFloat(value1, 0.05f, 0.001f)
      assertEqualsFloat(value2, 0.1f, 0.001f)
      assertNotEquals(value1, value2)
