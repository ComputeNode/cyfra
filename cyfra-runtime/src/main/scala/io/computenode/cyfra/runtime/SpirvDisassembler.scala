package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvDisassembler extends SpirvTool("spirv-dis") {

  def disassembleSpirv(shaderCode: ByteBuffer, disassembly: Disassembly): Option[String] = {
    disassembly match {
      case Enable(throwOnFail, params*) =>
        val disassemblyResult = tryGetDisassembleSpirv(shaderCode, params)
        disassemblyResult match {
          case Left(err) if throwOnFail => throw err
          case Left(err) => logger.warn(err.message)
            None
          case Right(value) => Some(value)
        }
      case Disable => logger.debug("SPIR-V disassembly is disabled.")
        None
    }
  }

  private def tryGetDisassembleSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvError, String] = {
    for
      executable <- findToolExecutable()
      cmd = Seq(executable.getAbsolutePath) ++ params.flatMap(_.asStringParam.split(" ")) ++ Seq("-")
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      result <- Either.cond(exitCode == 0, {
        logger.debug("SPIR-V disassembly succeeded.")
        stdout.toString
      }, SpirvDisassemblyFailed(exitCode, stderr.toString))
    yield result
  }

  sealed trait Disassembly

  case class Enable(throwOnFail: Boolean, settings: Param*) extends Disassembly

  private final case class SpirvDisassemblyFailed(exitCode: Int, stderr: String) extends SpirvError {
    def message: String =
      s"""SPIR-V disassembly failed with exit code $exitCode.
         |Disassembly errors:
         |$stderr""".stripMargin
  }

  case object Disable extends Disassembly

  object Enable:
    def apply(settings: Param*): Enable = Enable(false, settings *)
}
