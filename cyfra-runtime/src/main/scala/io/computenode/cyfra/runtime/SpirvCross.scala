package io.computenode.cyfra.runtime

import io.computenode.cyfra.runtime.SpirvDisassembler.executeSpirvCmd
import io.computenode.cyfra.runtime.SpirvValidator.findToolExecutable
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvCross extends SpirvTool("spirv-cross") {

  def crossCompileSpirv(shaderCode: ByteBuffer, crossCompilation: CrossCompilation): Option[String] = {
    crossCompilation match {
      case Enable(throwOnFail, params*) =>
        val crossCompilationRes = tryCrossCompileSpirv(shaderCode, params)
        crossCompilationRes match {
          case Left(err) if throwOnFail => throw err
          case Left(err) => logger.warn(err.message)
            None
          case Right(crossCompiledCode) => Some(crossCompiledCode)
        }
      case Disable => logger.debug("SPIR-V cross compilation is disabled.")
        None
    }
  }

  private def tryCrossCompileSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvError, String] = {
    for
      executable <- findToolExecutable()
      cmd = Seq(executable.getAbsolutePath) ++ Seq("-") ++ params.flatMap(_.asStringParam.split(" "))
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      result <- Either.cond(exitCode == 0, {
        logger.debug("SPIR-V cross compilation succeeded.")
        stdout.toString
      }, SpirvCrossCompilationFailed(exitCode, stderr.toString))
    yield result
  }

  sealed trait CrossCompilation

  case class Enable(throwOnFail: Boolean, settings: Param*) extends CrossCompilation

  private final case class SpirvCrossCompilationFailed(exitCode: Int, stderr: String) extends SpirvError {
    def message: String =
      s"""SPIR-V cross compilation failed with exit code $exitCode.
         |Cross errors:
         |$stderr""".stripMargin
  }

  object Enable:
    def apply(settings: Param*): Enable = Enable(false, settings *)

  case object Disable extends CrossCompilation
}
