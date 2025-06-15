package io.computenode.cyfra.spirvtools

import munit.FunSuite

import java.io.{ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.file.Files

class SpirvToolTest extends FunSuite {
  private def isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("win")

  class TestSpirvTool(toolName: String) extends SpirvTool(toolName) {
    def runFindTool(): Either[SpirvToolError, File] = findToolExecutable()

    def runExecuteCmd(input: ByteBuffer, cmd: Seq[String]): Either[SpirvToolError, (ByteArrayOutputStream, ByteArrayOutputStream, Int)] =
      executeSpirvCmd(input, cmd)
  }

  test("findToolExecutable returns Right when tool found in PATH") {
    val tmpDir = Files.createTempDirectory("spirvtooltest")
    val fakeTool = tmpDir.resolve("fake-tool")
    Files.createFile(fakeTool)
    fakeTool.toFile.setExecutable(true)

    val originalPath = sys.env.getOrElse("PATH", "")
    val newPath = tmpDir.toAbsolutePath.toString + File.pathSeparator + originalPath

    val tool = new TestSpirvTool("fake-tool") {
      override protected def findToolExecutable(): Either[SpirvToolError, File] = {
        val directories = newPath.split(File.pathSeparator)
        directories.map(dir => new File(dir, toolName)).find(file => file.exists() && file.canExecute) match {
          case Some(file) => Right(file)
          case None       => Left(SpirvToolNotFound(toolName))
        }
      }
    }

    val result = tool.runFindTool()
    assert(result.isRight)
    val file = result.getOrElse(fail("Should have found file"))
    assert(file.exists())
    assert(file.canExecute)

    Files.deleteIfExists(fakeTool)
    Files.deleteIfExists(tmpDir)
  }

  test("findToolExecutable returns Left when tool not found") {
    val tool = new TestSpirvTool("non-existent-tool")
    val result = tool.runFindTool()
    assert(result.isLeft)
    val error = result.left.getOrElse(fail("Should have error"))
    assert(error.getMessage.contains("non-existent-tool"))
  }

  if (!isWindows)
    test("executeSpirvCmd returns exit code and output streams on valid command") {
      val tool = new TestSpirvTool("cat")

      val inputBytes = "hello SPIR-V".getBytes("UTF-8")
      val byteBuffer = ByteBuffer.wrap(inputBytes)

      val cmd = Seq("cat")

      val result = tool.runExecuteCmd(byteBuffer, cmd)
      assert(result.isRight)

      val (outStream, errStream, exitCode) = result.getOrElse(fail("Execution failed"))
      val outputString = outStream.toString("UTF-8")

      assertEquals(exitCode, 0)
      assert(outputString == "hello SPIR-V")
      assertEquals(errStream.size(), 0)
    }

  test("executeSpirvCmd returns non-zero exit code on invalid command") {
    val tool = new TestSpirvTool("invalid-cmd")

    val byteBuffer = ByteBuffer.wrap("".getBytes("UTF-8"))
    val cmd = Seq("invalid-cmd")

    val result = tool.runExecuteCmd(byteBuffer, cmd)
    assert(result.isLeft)
    val error = result.left.getOrElse(fail("Should have error"))
    assert(error.getMessage.contains("Failed to execute SPIR-V command"))
  }

  test("dumpSpvToFile writes ByteBuffer content to file") {
    val tmpFile = Files.createTempFile("spirv-dump-test", ".spv")

    val data = "SPIRV binary data".getBytes("UTF-8")
    val buffer = ByteBuffer.wrap(data)

    val tmp = SpirvTool.ToFile(tmpFile)
    tmp.write(buffer)

    val fileBytes = Files.readAllBytes(tmpFile)
    assert(java.util.Arrays.equals(data, fileBytes))

    assert(buffer.position() == 0)

    Files.deleteIfExists(tmpFile)
  }

  test("Param.asStringParam returns correct string") {
    val param = SpirvTool.Param("test-value")
    assertEquals(param.asStringParam, "test-value")
  }
}
