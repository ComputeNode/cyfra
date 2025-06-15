package io.computenode.cyfra.spirvtools

import SpirvDisassembler.executeSpirvCmd
import SpirvValidator.findToolExecutable
import SpirvTool.Param
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Files

object SpirvCross extends SpirvTool("spirv-cross") {

  def crossCompileSpirv(shaderCode: ByteBuffer, crossCompilation: CrossCompilation): Option[String] = {
    crossCompilation match {
      case Enable(throwOnFail, resultSaveSetting, params) =>
        val crossCompilationRes = tryCrossCompileSpirv(shaderCode, params)
        crossCompilationRes match {
          case Left(err) if throwOnFail => throw err
          case Left(err) => logger.warn(err.message)
            None
          case Right(crossCompiledCode) =>
            resultSaveSetting match
              case NoSaving =>
              case ToFile(filePath) =>
                Files.write(filePath, crossCompiledCode.getBytes(StandardCharsets.UTF_8))
                logger.debug(s"Saved cross compiled shader code in $filePath.")
              case ToLogger => logger.debug(s"SPIR-V Cross Compilation result:\n$crossCompiledCode")
            Some(crossCompiledCode)
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

  case object ToLogger extends ResultSaveSetting

  case class Enable(throwOnFail: Boolean = false, resultSaveSetting: ResultSaveSetting = ToLogger, settings: Seq[Param] = Seq.empty) extends CrossCompilation

  final case class SpirvCrossCompilationFailed(exitCode: Int, stderr: String) extends SpirvError {
    def message: String =
      s"""SPIR-V cross compilation failed with exit code $exitCode.
         |Cross errors:
         |$stderr""".stripMargin
  }

  case object Disable extends CrossCompilation
}
