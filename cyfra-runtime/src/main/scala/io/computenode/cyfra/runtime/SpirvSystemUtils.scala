package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer
import java.nio.file.Path
import scala.sys.process.*
import scala.util.Using
import java.io.FileOutputStream

object SpirvSystemUtils {

  enum SupportedSpirVTools(val executableName: String):
    case Validator extends SupportedSpirVTools("spirv-val")
    case Optimizer extends SupportedSpirVTools("spirv-opt")

  enum SupportedSystem:
    case Windows
    case Linux
    case MacOS

  def getOS: Option[SupportedSystem] = {
    val os = System.getProperty("os.name").toLowerCase
    if (os.contains("win")) return Some(SupportedSystem.Windows)
    if (os.contains("mac")) return Some(SupportedSystem.MacOS)
    if (os.contains("linux")) return Some(SupportedSystem.Linux)
    logger.warn("This system does not support SPIRV-Tools.")
    None
  }

  private def getToolExecutableName(tool: SupportedSpirVTools, os: SupportedSystem): String = {
    os match {
      case SupportedSystem.Windows => tool.executableName + ".exe"
      case _ => tool.executableName
    }
  }

  def getToolExecutableFromPath(tool: SupportedSpirVTools, os: SupportedSystem): Option[String] = {
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

  def dumpSpvToFile(code: ByteBuffer, path: Path): Unit = {
    Using.resource(new FileOutputStream(path.toAbsolutePath.toString).getChannel) { fc =>
      fc.write(code)
    }
    code.rewind()
  }
}
