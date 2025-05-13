package io.computenode.cyfra.runtime

import java.nio.file.{Files, Path}
import scala.sys.process.*
import scala.util.Try

class SpirvValidationError(msg: String) extends RuntimeException(msg)

object SpirvValidator {
  private val disableValidatorEnv = "CYFRA_DISABLE_SPIRV_VALIDATION"

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

  def validateSpirv(spirvFilePath: Path): Unit = {
    val validatorExecutable: Option[String] = getOS.flatMap(getValidatorExecutableFromPath)
    if (validatorExecutable.isEmpty) return

    val process = Process(Seq(validatorExecutable.get, spirvFilePath.toAbsolutePath.toString))
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val processLogger = ProcessLogger(
      (out: String) => stdout.append(out + "\n"),
      (err: String) => stderr.append(err + "\n")
    )
    val exitCode = process.!(processLogger)
    exitCode match {
      case 0 =>
        println(s"SPIRV-Tools Validator succeeded!")
        if (stdout.nonEmpty) println(stdout.toString())
      case _ =>
        val exceptionMsg = s"SPIRV-Tools Validator failed with exit code $exitCode.\n" + "Validation errors:\n" + stderr.toString()
        throw new SpirvValidationError(exceptionMsg)
    }
  }

  def isSpirvValidationEnabled: Boolean = {
    sys.env.get(disableValidatorEnv) match
      case Some(value) =>
        value.toLowerCase match
          case "on" => false
          case _ => true
      case None => true
  }

  def corruptSpirvFile(originalSpirvFilePath: Path, corruptedSpirvFilePath: Path): Unit = {
    val originalBytes = Files.readAllBytes(originalSpirvFilePath)
    val corruptedBytes = originalBytes.clone()

    // Introduce a simple corruption by changing the first 4 bytes (magic number)
    corruptedBytes(0) = (corruptedBytes(0) ^ 0x01).toByte // XOR the first byte to break the magic number

    Files.write(corruptedSpirvFilePath, corruptedBytes)
    println(s"Corrupted SPIR-V file created at: ${corruptedSpirvFilePath.toString}")
  }
}
