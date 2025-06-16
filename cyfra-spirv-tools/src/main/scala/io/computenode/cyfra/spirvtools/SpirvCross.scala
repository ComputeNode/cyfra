package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvDisassembler.executeSpirvCmd
import io.computenode.cyfra.spirvtools.SpirvTool.{Ignore, Param, ToFile, ToLogger}
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvCross extends SpirvTool("spirv-cross"):

  def crossCompileSpirv(shaderCode: ByteBuffer, crossCompilation: CrossCompilation): Option[String] =
    crossCompilation match
      case Enable(throwOnFail, toolOutput, params) =>
        val crossCompilationRes = tryCrossCompileSpirv(shaderCode, params)
        crossCompilationRes match
          case Left(err) if throwOnFail => throw err
          case Left(err)                =>
            logger.warn(err.message)
            None
          case Right(crossCompiledCode) =>
            toolOutput match
              case Ignore                       =>
              case toFile @ SpirvTool.ToFile(_) =>
                toFile.write(crossCompiledCode)
                logger.debug(s"Saved cross compiled shader code in ${toFile.filePath}.")
              case ToLogger => logger.debug(s"SPIR-V Cross Compilation result:\n$crossCompiledCode")
            Some(crossCompiledCode)
      case Disable =>
        logger.debug("SPIR-V cross compilation is disabled.")
        None

  private def tryCrossCompileSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvToolError, String] =
    val cmd = Seq(toolName) ++ Seq("-") ++ params.flatMap(_.asStringParam.split(" "))
    for
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      result <- Either.cond(
        exitCode == 0, {
          logger.debug("SPIR-V cross compilation succeeded.")
          stdout.toString
        },
        SpirvToolCrossCompilationFailed(exitCode, stderr.toString),
      )
    yield result

  sealed trait CrossCompilation

  case class Enable(throwOnFail: Boolean = false, toolOutput: ToFile | Ignore.type | ToLogger.type = ToLogger, settings: Seq[Param] = Seq.empty)
      extends CrossCompilation

  final case class SpirvToolCrossCompilationFailed(exitCode: Int, stderr: String) extends SpirvToolError:
    def message: String =
      s"""SPIR-V cross compilation failed with exit code $exitCode.
         |Cross errors:
         |$stderr""".stripMargin

  case object Disable extends CrossCompilation
