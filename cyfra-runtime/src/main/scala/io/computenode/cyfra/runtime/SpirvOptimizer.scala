package io.computenode.cyfra.runtime

import io.computenode.cyfra.runtime.SpirvDisassembler.executeSpirvCmd
import io.computenode.cyfra.runtime.SpirvValidator.findToolExecutable
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvOptimizer extends SpirvTool("spirv-opt") {

  def optimizeSpirv(shaderCode: ByteBuffer, optimization: Optimization): ByteBuffer = {
    optimization match {
      case Enable(throwOnFail, params*) =>
        val validationRes = tryGetOptimizeSpirv(shaderCode, params)
        validationRes match {
          case Left(err) if throwOnFail => throw err
          case Left(err) => logger.warn(err.message)
            shaderCode
          case Right(optimizedShaderCode) => optimizedShaderCode
        }
      case Disable => logger.debug("SPIR-V optimization is disabled.")
        shaderCode
    }
  }

  private def tryGetOptimizeSpirv(shaderCode: ByteBuffer, params: Seq[Param]): Either[SpirvError, ByteBuffer] = {
    for
      executable <- findToolExecutable()
      cmd = Seq(executable.getAbsolutePath) ++ params.flatMap(_.asStringParam.split(" ")) ++ Seq("-", "-o", "-")
      (stdout, stderr, exitCode) <- executeSpirvCmd(shaderCode, cmd)
      result <- Either.cond(exitCode == 0, {
        logger.debug("SPIR-V optimization succeeded.")
        val optimized = toDirectBuffer(ByteBuffer.wrap(stdout.toByteArray))
        optimized
      }, SpirvOptimizationFailed(exitCode, stderr.toString))
    yield result
  }

  private def toDirectBuffer(buf: ByteBuffer): ByteBuffer = {
    val direct = ByteBuffer.allocateDirect(buf.remaining())
    direct.put(buf)
    direct.flip()
    direct
  }

  sealed trait Optimization

  case class Enable(throwOnFail: Boolean, settings: Param*) extends Optimization

  private final case class SpirvOptimizationFailed(exitCode: Int, stderr: String) extends SpirvError {
    def message: String =
      s"""SPIR-V optimization failed with exit code $exitCode.
         |Optimizer errors:
         |$stderr""".stripMargin
  }

  object Enable:
    def apply(settings: Param*): Enable = Enable(false, settings *)

  case object Disable extends Optimization
}