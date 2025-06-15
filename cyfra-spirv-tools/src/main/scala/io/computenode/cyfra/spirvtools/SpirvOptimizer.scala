package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvDisassembler.executeSpirvCmd
import io.computenode.cyfra.spirvtools.SpirvTool.Param
import io.computenode.cyfra.spirvtools.SpirvValidator.findToolExecutable
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvOptimizer extends SpirvTool("spirv-opt") {

  def optimizeSpirv(shaderCode: ByteBuffer, optimization: Optimization): Option[ByteBuffer] =
    optimization match {
      case Enable(throwOnFail, resultSaveSetting, params) =>
        val optimizationRes = tryGetOptimizeSpirv(shaderCode, params)
        optimizationRes match {
          case Left(err) if throwOnFail => throw err
          case Left(err)                =>
            logger.warn(err.message)
            None
          case Right(optimizedShaderCode) =>
            resultSaveSetting match {
              case NoSaving         =>
              case ToFile(filePath) =>
                SpirvTool.dumpSpvToFile(optimizedShaderCode, filePath)
                logger.debug(s"Saved optimized shader code in $filePath.")
            }
            Some(optimizedShaderCode)
        }
      case Disable =>
        logger.debug("SPIR-V optimization is disabled.")
        None
    }

  private def tryGetOptimizeSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvError, ByteBuffer] =
    for
      executable <- findToolExecutable()
      cmd = Seq(executable.getAbsolutePath) ++ params.flatMap(_.asStringParam.split(" ")) ++ Seq("-", "-o", "-")
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      result <- Either.cond(
        exitCode == 0, {
          logger.debug("SPIR-V optimization succeeded.")
          val optimized = toDirectBuffer(ByteBuffer.wrap(stdout.toByteArray))
          optimized
        },
        SpirvOptimizationFailed(exitCode, stderr.toString),
      )
    yield result

  private def toDirectBuffer(buf: ByteBuffer): ByteBuffer = {
    val direct = ByteBuffer.allocateDirect(buf.remaining())
    direct.put(buf)
    direct.flip()
    direct
  }

  sealed trait Optimization

  case class Enable(throwOnFail: Boolean = false, resultSaveSetting: ResultSaveSetting = NoSaving, settings: Seq[Param] = Seq.empty)
      extends Optimization

  final case class SpirvOptimizationFailed(exitCode: Int, stderr: String) extends SpirvError {
    def message: String =
      s"""SPIR-V optimization failed with exit code $exitCode.
         |Optimizer errors:
         |$stderr""".stripMargin
  }

  case object Disable extends Optimization
}
