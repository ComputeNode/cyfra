package io.computenode.cyfra.runtime

import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import scala.sys.process.*
import scala.util.{Try, Using}

class SpirvValidationError(msg: String) extends RuntimeException(msg)

object SpirvValidator {

  private enum SupportedSystem:
    case Windows
    case Linux
    case MacOS


  private def getOS: Option[SupportedSystem] = {
    val os = System.getProperty("os.name").toLowerCase
    if (os.contains("win")) return Some(SupportedSystem.Windows)
    if (os.contains("mac")) return Some(SupportedSystem.MacOS)
    if (os.contains("linux")) return Some(SupportedSystem.Linux)
    println("Warning: This system does not support SPIRV-Tools Validator.")
    None
  }

  private def getValidatorNameForCurrentOs(os: SupportedSystem): String = {
    os match {
      case SupportedSystem.Windows => "spirv-val.exe"
      case SupportedSystem.Linux => "spirv-val"
      case SupportedSystem.MacOS => "spirv-val"
    }
  }

  private def getValidatorExecutableFromPath(os: SupportedSystem): Option[String] = {
    val checker = os match
      case SupportedSystem.Windows => "where"
      case _ => "which"

    val validatorName = getValidatorNameForCurrentOs(os)

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
        println(s"Warning: SPIRV-Tools Validator wasn't found in system path.\n${stderr.toString()}")
        None
  }

  def validateSpirv(shaderCode: ByteBuffer): Unit = {
    val tmpSpirvPath: Path = Files.createTempFile("tmp_cyfra_file", ".spv")

    try {
      getOS.flatMap(getValidatorExecutableFromPath) match {
        case Some(validatorExecutable) =>
          dumpSpvToFile(shaderCode, tmpSpirvPath)

          val stdout = new StringBuilder
          val stderr = new StringBuilder

          val processLogger = ProcessLogger(
            (out: String) => stdout.append(out).append("\n"),
            (err: String) => stderr.append(err).append("\n")
          )

          val process = Process(Seq(validatorExecutable, tmpSpirvPath.toAbsolutePath.toString))
          val exitCode = process.!(processLogger)

          exitCode match {
            case 0 =>
              println("SPIRV-Tools Validator succeeded!")
              if (stdout.nonEmpty) println(stdout.toString())
            case _ =>
              val errorMsg = s"SPIRV-Tools Validator failed with exit code $exitCode.\n" +
                "Validation errors:\n" + stderr.toString()
              throw new SpirvValidationError(errorMsg)
          }

        case None =>
          println("Validator executable not found.")
      }
    } finally {
      Try(Files.deleteIfExists(tmpSpirvPath)).recover {
        case e: Exception =>
          println(s"Failed to delete tmp spv file: ${e.getMessage}")
      }
    }
  }

  private def dumpSpvToFile(code: ByteBuffer, path: Path): Unit = {
    Using.resource(new FileOutputStream(path.toAbsolutePath.toString).getChannel) { fc =>
      fc.write(code)
    }
    code.rewind()
  }
}
