package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvDisassembler.executeSpirvCmd
import io.computenode.cyfra.spirvtools.SpirvTool.{Ignore, Param, ToFile}
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvOptimizer extends SpirvTool("spirv-opt"):

  def optimizeSpirv(shaderCode: ByteBuffer, optimization: Optimization): Option[ByteBuffer] =
    optimization match
      case Enable(throwOnFail, toolOutput, params) =>
        val optimizationRes = tryGetOptimizeSpirv(shaderCode, params)
        optimizationRes match
          case Left(err) if throwOnFail => throw err
          case Left(err)                =>
            logger.warn(err.message)
            None
          case Right(optimizedShaderCode) =>
            toolOutput match
              case SpirvTool.Ignore             =>
              case toFile @ SpirvTool.ToFile(_, _) =>
                toFile.write(optimizedShaderCode)
                logger.debug(s"Saved optimized shader code in ${toFile.filePath}.")
            Some(optimizedShaderCode)
      case Disable =>
        logger.debug("SPIR-V optimization is disabled.")
        None

  private def tryGetOptimizeSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvToolError, ByteBuffer] =
    val cmd = Seq(toolName) ++ params.flatMap(_.asStringParam.split(" ")) ++ Seq("-", "-o", "-")
    for
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      result <- Either.cond(
        exitCode == 0, {
          logger.debug("SPIR-V optimization succeeded.")
          val optimized = toDirectBuffer(ByteBuffer.wrap(stdout.toByteArray))
          optimized
        },
        SpirvToolOptimizationFailed(exitCode, stderr.toString),
      )
    yield result

  private def toDirectBuffer(buf: ByteBuffer): ByteBuffer =
    val direct = ByteBuffer.allocateDirect(buf.remaining())
    direct.put(buf)
    direct.flip()
    direct

  sealed trait Optimization

  case class Enable(throwOnFail: Boolean = false, toolOutput: ToFile | Ignore.type = Ignore, settings: Seq[Param] = Seq.empty) extends Optimization

  final case class SpirvToolOptimizationFailed(exitCode: Int, stderr: String) extends SpirvToolError:
    def message: String =
      s"""SPIR-V optimization failed with exit code $exitCode.
         |Optimizer errors:
         |$stderr""".stripMargin

  case object Disable extends Optimization
