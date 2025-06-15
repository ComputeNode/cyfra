package io.computenode.cyfra.spirvtools

import SpirvDisassembler.{executeSpirvCmd, findToolExecutable}
import SpirvTool.Param
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvValidator extends SpirvTool("spirv-val") {

  def validateSpirv(shaderCode: ByteBuffer, validation: Validation): Unit = {
    validation match {
      case Enable(throwOnFail, params) =>
        val validationRes = tryValidateSpirv(shaderCode, params)
        validationRes match {
          case Left(err) if throwOnFail => throw err
          case Left(err) => logger.warn(err.message)
          case Right(_) => ()
        }
      case Disable => logger.debug("SPIR-V validation is disabled.")
    }
  }

  private def tryValidateSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvError, Unit] = {
    for
      executable <- findToolExecutable()
      cmd = Seq(executable.getAbsolutePath) ++ params.flatMap(_.asStringParam.split(" ")) ++ Seq("-")
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      _ <- Either.cond(exitCode == 0, logger.debug("SPIR-V validation succeeded."), SpirvValidationFailed(exitCode, stderr.toString()))
    yield ()
  }

  sealed trait Validation

  case class Enable(throwOnFail: Boolean = false, settings: Seq[Param] = Seq.empty) extends Validation

  final case class SpirvValidationFailed(exitCode: Int, stderr: String) extends SpirvError {
    def message: String =
      s"""SPIR-V validation failed with exit code $exitCode.
         |Validation errors:
         |$stderr""".stripMargin
  }

  case object Disable extends Validation
}
