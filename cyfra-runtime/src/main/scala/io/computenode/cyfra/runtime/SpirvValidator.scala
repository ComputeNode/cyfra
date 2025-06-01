package io.computenode.cyfra.runtime

import io.computenode.cyfra.runtime.SpirvDisassembler.executeSpirvCmd
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvValidator extends SpirvTool {

  override type SpirvError = SpirvValidationError
  protected override val toolName: SupportedSpirVTools = SupportedSpirVTools.Validator

  def validateSpirv(shaderCode: ByteBuffer, enableValidation: Validation): Unit = {
    enableValidation match {
      case Disable =>
      case Enable(settings*) =>
        getOS.flatMap { os =>
          getToolExecutableFromPath(toolName, os)
        } match {
          case None => logger.warn("Shader code will not be validated.")
          case Some(executable) =>
            val cmd = Seq(executable) ++ settings.flatMap(_.asStringParam.split(" ")) ++ Seq("-")
            val (outputStream, errorStream, exitCode) = executeSpirvCmd(shaderCode, cmd)

            if (exitCode == 0) {
              logger.debug("SPIRV-Tools Validator succeeded.")
              if (outputStream.toString().nonEmpty) logger.debug(outputStream.toString())
            } else {
              throw SpirvValidationError(s"SPIRV-Tools Validator failed with exit code $exitCode.\n" +
                s"Validation errors:\n${errorStream.toString()}")
            }
        }
    }
  }

  override protected def createError(message: String): SpirvValidationError =
    SpirvValidationError(message)

  sealed trait Validation

  case class SpirvValidationError(msg: String) extends RuntimeException(msg)

  case class TargetEnv(version: String) extends ParamWithArgs("--target-env", version)

  case class Enable(settings: Param*) extends Validation

  case object Disable extends Validation
}
