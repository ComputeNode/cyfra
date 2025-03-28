package io.computenode.cyfra.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.joml.Vector3i
import java.nio.{ByteBuffer, ByteOrder}
import org.lwjgl.BufferUtils
import io.computenode.cyfra.vulkan.compute.Shader.loadShader
import scala.util.{Success, Failure, Try}

class ComputeContextApiTest extends AnyFunSuite with BeforeAndAfterEach with Matchers {
  private var context: ComputeContext = _

  override def beforeEach(): Unit = {
    println("Initializing ComputeContext...")
    context = new ComputeContext(enableValidation = true)
  }

  override def afterEach(): Unit = {
    if (context != null) context.close()
  }

  test("execute simple shader pipeline") {
    try {
      val shaderBytes = loadShader("copy_test.spv").array()

      var i = 5 // Skip header
      while (i < shaderBytes.length / 4) {
        val word = ((shaderBytes(i * 4) & 0xFF)) |
          ((shaderBytes(i * 4 + 1) & 0xFF) << 8) |
          ((shaderBytes(i * 4 + 2) & 0xFF) << 16) |
          ((shaderBytes(i * 4 + 3) & 0xFF) << 24)

        val opCode = word & 0xFFFF
        val wordCount = (word >> 16) & 0xFFFF

        if (opCode == 0x3E) {
          println(s"Found OpStore at word $i")
        }

        i += wordCount
      }
    } catch {
      case e: Exception => println(s"Error parsing shader: ${e.getMessage}")
    }

    println("Disassembling SPIR-V shader with spirv-dis:")
    try {
      val processBuilder = new ProcessBuilder("spirv-dis", "src/test/resources/copy_test.spv")
      val process = processBuilder.start()
      val output = scala.io.Source.fromInputStream(process.getInputStream).mkString
      println(output.split("\n").filter(_.contains("Store")).mkString("\n"))
      process.waitFor()
    } catch {
      case e: Exception => println("Could not disassemble shader: " + e.getMessage)
    }

    println("Loading SPIR-V shader 'copy_test.spv'...")
    val spirvCode = loadShader("copy_test.spv")

    println("Creating shader layout information...")
    val layoutInfo = LayoutInfo(Seq(
      LayoutSet(0, Seq(Binding(0, 4))),
      LayoutSet(1, Seq(Binding(0, 4)))
    ))

    println("Creating shader with ComputeContext...")
    val shaderTry = context.createShader(
      spirvCode,
      new Vector3i(128, 1, 1),
      layoutInfo
    )
    shaderTry.isSuccess shouldBe true

    val shader = shaderTry.get

    println("Creating compute pipeline...")
    val pipelineTry = context.createPipeline(shader)
    pipelineTry.isSuccess shouldBe true

    val pipeline = pipelineTry.get

    val dataSize = 8
    println(s"Creating input buffer with size ${4 * dataSize} bytes...")
    val inputBufferTry = context.createBuffer(4 * dataSize, isHostVisible = true)
    inputBufferTry.isSuccess shouldBe true

    val inputBuffer = inputBufferTry.get

    val inputData = BufferUtils.createByteBuffer(4 * dataSize)
    for (i <- 0 until dataSize) {
      inputData.putInt(i)
    }
    inputData.flip()

    println("Copying data to input buffer...")
    val copyResult = inputBuffer.copyFrom(inputData)
    copyResult.isSuccess shouldBe true

    println(s"Creating output buffer with size ${4 * dataSize} bytes...")
    val outputBufferTry = context.createBuffer(4 * dataSize, isHostVisible = true)
    outputBufferTry.isSuccess shouldBe true

    val outputBuffer = outputBufferTry.get

    println("Executing compute pipeline...")
    val executeTry = context.execute(pipeline, Seq(inputBuffer), Seq(outputBuffer), dataSize)
    executeTry.isSuccess shouldBe true

    println("Reading results from output buffer...")
    val resultTry = outputBuffer.copyToHost()
    resultTry.isSuccess shouldBe true

    val result = resultTry.get
    result.order(ByteOrder.nativeOrder())
    result.rewind()

    println("Verifying computation results...")
    for (i <- 0 until dataSize) {
      val expectedVal = i + 10000
      val actualVal = result.getInt()
      actualVal should be(expectedVal)
    }

    println("Cleaning up resources...")
    outputBuffer.close()
    inputBuffer.close()
    pipeline.close()
    shader.close()
  }
}