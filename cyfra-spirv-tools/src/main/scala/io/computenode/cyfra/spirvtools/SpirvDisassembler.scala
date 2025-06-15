package io.computenode.cyfra.spirvtools

import SpirvTool.Param
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Files

object SpirvDisassembler extends SpirvTool("spirv-dis") {

  def disassembleSpirv(shaderCode: ByteBuffer, disassembly: Disassembly): Option[String] = {
    disassembly match {
      case Enable(throwOnFail, resultSaveSetting, params) =>
        val disassemblyResult = tryGetDisassembleSpirv(shaderCode, params)
        disassemblyResult match {
          case Left(err) if throwOnFail => throw err
          case Left(err) => logger.warn(err.message)
            None
          case Right(disassembledShader) =>
            resultSaveSetting match {
              case NoSaving =>
              case ToFile(filePath) =>
                Files.write(filePath, disassembledShader.getBytes(StandardCharsets.UTF_8))
                logger.debug(s"Saved disassembled shader code in $filePath.")
              case ToLogger => logger.debug(s"SPIR-V Assembly:\n$disassembledShader")
            }
            Some(disassembledShader)
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

  final case class SpirvDisassemblyFailed(exitCode: Int, stderr: String) extends SpirvError {
    def message: String =
      s"""SPIR-V disassembly failed with exit code $exitCode.
         |Disassembly errors:
         |$stderr""".stripMargin
  }

  case object ToLogger extends ResultSaveSetting

  sealed trait Disassembly

  case class Enable(throwOnFail: Boolean = false, resultSaveSetting: ResultSaveSetting = ToLogger, settings: Seq[Param] = Seq.empty) extends Disassembly

  case object Disable extends Disassembly
}
