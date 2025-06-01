package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.file.Path
import scala.sys.process.{ProcessIO, ProcessLogger, stringSeqToProcess, *}
import scala.util.Using

abstract class SpirvTool {
  type SpirvError <: RuntimeException
  protected val toolName: SupportedSpirVTools

  def dumpSpvToFile(code: ByteBuffer, path: Path): Unit = {
    Using.resource(new FileOutputStream(path.toAbsolutePath.toString).getChannel) { fc =>
      fc.write(code)
    }
    code.rewind()
  }

  protected def createError(msg: String): SpirvError

  protected def getOS: Option[SupportedSystem] = {
    val os = System.getProperty("os.name").toLowerCase
    if (os.contains("win")) return Some(SupportedSystem.Windows)
    if (os.contains("mac")) return Some(SupportedSystem.MacOS)
    if (os.contains("linux")) return Some(SupportedSystem.Linux)
    logger.warn("This system does not support SPIRV-Tools.")
    None
  }

  protected def getToolExecutableFromPath(tool: SupportedSpirVTools, os: SupportedSystem): Option[String] = {
    val checker = os match
      case SupportedSystem.Windows => "where"
      case _ => "which"

    val validatorName = getToolExecutableName(tool, os)

    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val processLogger = ProcessLogger(
      (out: String) => stdout.append(out + "\n"),
      (err: String) => stderr.append(err + "\n")
    )

    val exitCode = Process(Seq(checker, validatorName)).!(processLogger)
    exitCode match
      case 0 => Some(validatorName)
      case _ =>
        logger.warn(s"SPIRV-Tools ${tool.executableName} wasn't found in system path.")
        if (stderr.toString().nonEmpty) logger.warn(s"${stderr.toString()}")
        None
  }

  private def getToolExecutableName(tool: SupportedSpirVTools, os: SupportedSystem): String = {
    os match {
      case SupportedSystem.Windows => tool.executableName + ".exe"
      case _ => tool.executableName
    }
  }

  protected def executeSpirvCmd(shaderCode: ByteBuffer, cmd: Seq[String]): (ByteArrayOutputStream, ByteArrayOutputStream, Int) = {
    val inputBytes = {
      val arr = new Array[Byte](shaderCode.remaining())
      shaderCode.get(arr)
      shaderCode.rewind()
      arr
    }

    val inputStream = new ByteArrayInputStream(inputBytes)
    val outputStream = new ByteArrayOutputStream()
    val errorStream = new ByteArrayOutputStream()

    val processIO = new ProcessIO(
      in => {
        try {
          val buf = new Array[Byte](1024)
          var len = inputStream.read(buf)
          while (len != -1) {
            in.write(buf, 0, len)
            len = inputStream.read(buf)
          }
        } catch {
          case e: Exception => throw createError("Writing to stdin failed.\n" + e)
        } finally {
          in.close()
        }
      },
      out => {
        try {
          val buf = new Array[Byte](1024)
          var len = out.read(buf)
          while (len != -1) {
            outputStream.write(buf, 0, len)
            len = out.read(buf)
          }
        } catch {
          case e: Exception =>
            throw createError("Reading from stdout failed.\n" + e)
        } finally {
          out.close()
        }
      },
      err => {
        try {
          val buf = new Array[Byte](1024)
          var len = err.read(buf)
          while (len != -1) {
            errorStream.write(buf, 0, len)
            len = err.read(buf)
          }
        } catch {
          case e: Exception =>
            throw createError("Reading from stderr failed.\n" + e)
        } finally {
          err.close()
        }
      }
    )

    val process = cmd.run(processIO)
    val exitCode = process.exitValue()
    (outputStream, errorStream, exitCode)
  }

  trait Param:
    def asStringParam: String

  trait FlagParam(flag: String) extends Param:
    override def asStringParam: String = flag

  trait ParamWithArgs(param: String, args: String*) extends Param:
    override def asStringParam: String = param + args.mkString(" ", " ", "")

  protected enum SupportedSpirVTools(val executableName: String):
    case Validator extends SupportedSpirVTools("spirv-val")
    case Optimizer extends SupportedSpirVTools("spirv-opt")
    case Disassembler extends SupportedSpirVTools("spirv-dis")

  protected enum SupportedSystem:
    case Windows
    case Linux
    case MacOS
}
