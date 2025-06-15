package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.utility.Logger.logger

import java.io.*
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.sys.process.{ProcessIO, stringSeqToProcess}
import scala.util.{Try, Using}

abstract class SpirvTool(protected val toolName: String) {

  protected def findToolExecutable(): Either[SpirvError, File] = {
    val pathEnv = sys.env.getOrElse("PATH", "")
    val pathSeparator = File.pathSeparator
    val directories = pathEnv.split(pathSeparator)

    directories.map(dir => new File(dir, toolName)).find(file => file.exists() && file.canExecute) match {
      case Some(file) => logger.debug(s"Found SPIR-V tool: ${file.getAbsolutePath}")
        Right(file)
      case None =>
        Left(SpirvToolNotFound(toolName))
    }
  }

  protected trait ResultSaveSetting

  case object NoSaving extends ResultSaveSetting

  case class ToFile(filePath: Path) extends ResultSaveSetting {
    require(filePath != null, "filePath must not be null")
    Option(filePath.getParent).foreach { dir =>
      if (!Files.exists(dir)) {
        Files.createDirectories(dir)
        logger.debug(s"Created output directory: $dir")
      }
    }
  }

  protected def executeSpirvCmd(shaderCode: ByteBuffer, cmd: Seq[String]): Either[SpirvError, (ByteArrayOutputStream, ByteArrayOutputStream, Int)] = {
    logger.debug(s"SPIR-V cmd $cmd.")
    val inputBytes = {
      val arr = new Array[Byte](shaderCode.remaining())
      shaderCode.get(arr)
      shaderCode.rewind()
      arr
    }
    val inputStream = new ByteArrayInputStream(inputBytes)
    val outputStream = new ByteArrayOutputStream()
    val errorStream = new ByteArrayOutputStream()

    def safeIOCopy(inStream: InputStream, outStream: OutputStream, description: String): Either[SpirvToolIOError, Unit] = {
      @tailrec
      def loopOverBuffer(buf: Array[Byte]): Unit = {
        val len = inStream.read(buf)
        if (len == -1) ()
        else {
          outStream.write(buf, 0, len)
          loopOverBuffer(buf)
        }
      }

      Using.Manager { use =>
        val in = use(inStream)
        val out = use(outStream)
        val buf = new Array[Byte](1024)
        loopOverBuffer(buf)
        out.flush()
      }.toEither.left.map(e => SpirvToolIOError(s"$description failed: ${e.getMessage}"))
    }

    def createProcessIO(): Either[SpirvError, ProcessIO] = {
      val inHandler: OutputStream => Unit =
        in => safeIOCopy(inputStream, in, "Writing to stdin") match {
        case Left(err) => SpirvToolIOError(s"Failed to create ProcessIO: ${err.getMessage}")
        case Right(_) => ()
      }

      val outHandler: InputStream => Unit =
        out => safeIOCopy(out, outputStream, "Reading stdout") match {
        case Left(err) => SpirvToolIOError(s"Failed to create ProcessIO: ${err.getMessage}")
        case Right(_) => ()
      }

      val errHandler: InputStream => Unit =
        err => safeIOCopy(err, errorStream, "Reading stderr") match {
        case Left(err) => SpirvToolIOError(s"Failed to create ProcessIO: ${err.getMessage}")
        case Right(_) => ()
      }

      Try {
        new ProcessIO(inHandler, outHandler, errHandler)
      }.toEither.left.map(e => SpirvToolIOError(s"Failed to create ProcessIO: ${e.getMessage}"))
    }

    for {
      processIO <- createProcessIO()
      process <- Try(cmd.run(processIO)).toEither.left.map(ex => SpirvCommandExecutionFailed(s"Failed to execute SPIR-V command: ${ex.getMessage}"))}
    yield {
      (outputStream, errorStream, process.exitValue())
    }
  }

  trait SpirvError extends RuntimeException {
    def message: String

    override def getMessage: String = message
  }

  final case class SpirvToolNotFound(toolName: String) extends SpirvError {
    def message: String = s"Tool '$toolName' not found in PATH."
  }

  final case class SpirvCommandExecutionFailed(details: String) extends SpirvError {
    def message: String = s"SPIR-V command execution failed: $details"
  }

  final case class SpirvToolIOError(details: String) extends SpirvError {
    def message: String = s"SPIR-V command encountered IO error: $details"
  }
}

object SpirvTool {
  def dumpSpvToFile(code: ByteBuffer, path: Path): Unit = {
    Using.resource(new FileOutputStream(path.toAbsolutePath.toString).getChannel) { fc =>
      fc.write(code)
    }
    code.rewind()
  }

  case class Param(value: String):
    def asStringParam: String = value
}