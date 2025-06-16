package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.utility.Logger.logger

import java.io.*
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.sys.process.{ProcessIO, stringSeqToProcess}
import scala.util.{Try, Using}

abstract class SpirvTool(protected val toolName: String):

  protected def executeSpirvCmd(
    shaderCode: ByteBuffer,
    cmd: Seq[String],
  ): Either[SpirvToolError, (ByteArrayOutputStream, ByteArrayOutputStream, Int)] =
    logger.debug(s"SPIR-V cmd $cmd.")
    val inputBytes =
      val arr = new Array[Byte](shaderCode.remaining())
      shaderCode.get(arr)
      shaderCode.rewind()
      arr
    val inputStream = new ByteArrayInputStream(inputBytes)
    val outputStream = new ByteArrayOutputStream()
    val errorStream = new ByteArrayOutputStream()

    def safeIOCopy(inStream: InputStream, outStream: OutputStream, description: String): Either[SpirvToolIOError, Unit] =
      @tailrec
      def loopOverBuffer(buf: Array[Byte]): Unit =
        val len = inStream.read(buf)
        if len == -1 then ()
        else
          outStream.write(buf, 0, len)
          loopOverBuffer(buf)

      Using
        .Manager { use =>
          val in = use(inStream)
          val out = use(outStream)
          val buf = new Array[Byte](1024)
          loopOverBuffer(buf)
          out.flush()
        }
        .toEither
        .left
        .map(e => SpirvToolIOError(s"$description failed: ${e.getMessage}"))

    def createProcessIO(): Either[SpirvToolError, ProcessIO] =
      val inHandler: OutputStream => Unit =
        in =>
          safeIOCopy(inputStream, in, "Writing to stdin") match
            case Left(err) => SpirvToolIOError(s"Failed to create ProcessIO: ${err.getMessage}")
            case Right(_)  => ()

      val outHandler: InputStream => Unit =
        out =>
          safeIOCopy(out, outputStream, "Reading stdout") match
            case Left(err) => SpirvToolIOError(s"Failed to create ProcessIO: ${err.getMessage}")
            case Right(_)  => ()

      val errHandler: InputStream => Unit =
        err =>
          safeIOCopy(err, errorStream, "Reading stderr") match
            case Left(err) => SpirvToolIOError(s"Failed to create ProcessIO: ${err.getMessage}")
            case Right(_)  => ()

      Try {
        new ProcessIO(inHandler, outHandler, errHandler)
      }.toEither.left.map(e => SpirvToolIOError(s"Failed to create ProcessIO: ${e.getMessage}"))

    for
      processIO <- createProcessIO()
      process <- Try(cmd.run(processIO)).toEither.left.map(ex => SpirvToolCommandExecutionFailed(s"Failed to execute SPIR-V command: ${ex.getMessage}"))
    yield (outputStream, errorStream, process.exitValue())

  trait SpirvToolError extends RuntimeException:
    def message: String

    override def getMessage: String = message

  final case class SpirvToolNotFound(toolName: String) extends SpirvToolError:
    def message: String = s"Tool '$toolName' not found in PATH."

  final case class SpirvToolCommandExecutionFailed(details: String) extends SpirvToolError:
    def message: String = s"SPIR-V command execution failed: $details"

  final case class SpirvToolIOError(details: String) extends SpirvToolError:
    def message: String = s"SPIR-V command encountered IO error: $details"


object SpirvTool:
  sealed trait ToolOutput

  case class Param(value: String):
    def asStringParam: String = value

  case class ToFile(filePath: Path) extends ToolOutput:
    require(filePath != null, "filePath must not be null")

    def write(outputToSave: String | ByteBuffer): Unit =
      Option(filePath.getParent).foreach { dir =>
        if !Files.exists(dir) then
          Files.createDirectories(dir)
          logger.debug(s"Created output directory: $dir")
        outputToSave match
          case stringOutput: String   => Files.write(filePath, stringOutput.getBytes(StandardCharsets.UTF_8))
          case byteBuffer: ByteBuffer => dumpByteBufferToFile(byteBuffer, filePath)
      }

    private def dumpByteBufferToFile(code: ByteBuffer, path: Path): Unit =
      Using.resource(new FileOutputStream(path.toAbsolutePath.toString).getChannel) { fc =>
        fc.write(code)
      }
      code.rewind()

  case object ToLogger extends ToolOutput

  case object Ignore extends ToolOutput
