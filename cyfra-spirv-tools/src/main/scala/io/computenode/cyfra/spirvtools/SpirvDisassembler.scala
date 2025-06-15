package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvTool.{Ignore, Param, ToFile, ToLogger}
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Files

object SpirvDisassembler extends SpirvTool("spirv-dis") {

  def disassembleSpirv(shaderCode: ByteBuffer, disassembly: Disassembly): Option[String] =
    disassembly match {
      case Enable(throwOnFail, toolOutput, params) =>
        val disassemblyResult = tryGetDisassembleSpirv(shaderCode, params)
        disassemblyResult match {
          case Left(err) if throwOnFail => throw err
          case Left(err)                =>
            logger.warn(err.message)
            None
          case Right(disassembledShader) =>
            toolOutput match {
              case Ignore                       =>
              case toFile @ SpirvTool.ToFile(_) =>
                toFile.write(disassembledShader)
                logger.debug(s"Saved disassembled shader code in ${toFile.filePath}.")
              case ToLogger => logger.debug(s"SPIR-V Assembly:\n$disassembledShader")
            }
            Some(disassembledShader)
        }
      case Disable =>
        logger.debug("SPIR-V disassembly is disabled.")
        None
    }

  private def tryGetDisassembleSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvToolError, String] =
    for
      executable <- findToolExecutable()
      cmd = Seq(executable.getAbsolutePath) ++ params.flatMap(_.asStringParam.split(" ")) ++ Seq("-")
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      result <- Either.cond(
        exitCode == 0, {
          logger.debug("SPIR-V disassembly succeeded.")
          stdout.toString
        },
        SpirvToolDisassemblyFailed(exitCode, stderr.toString),
      )
    yield result

  sealed trait Disassembly

  final case class SpirvToolDisassemblyFailed(exitCode: Int, stderr: String) extends SpirvToolError {
    def message: String =
      s"""SPIR-V disassembly failed with exit code $exitCode.
         |Disassembly errors:
         |$stderr""".stripMargin
  }

  case class Enable(throwOnFail: Boolean = false, toolOutput: ToFile | Ignore.type | ToLogger.type = ToLogger, settings: Seq[Param] = Seq.empty)
      extends Disassembly

  case object Disable extends Disassembly
}
