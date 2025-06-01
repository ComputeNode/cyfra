package io.computenode.cyfra.runtime

import io.computenode.cyfra.runtime.SpirvDisassembler.executeSpirvCmd
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvOptimizer extends SpirvTool {

  override type SpirvError = SpirvOptimizationError
  protected override val toolName: SupportedSpirVTools = SupportedSpirVTools.Optimizer

  def getOptimizedSpirv(shaderCode: ByteBuffer, optimization: Optimization): Option[ByteBuffer] = {
    optimization match {
      case Disable => None
      case Enable(settings*) =>
        getOS.flatMap { os =>
          getToolExecutableFromPath(
            toolName, os)
        } match {
          case None =>
            logger.warn("Shader code will not be optimized.")
            None
          case Some(executable) =>
            val cmd = Seq(executable) ++ settings.flatMap(_.asStringParam.split(" ")) ++ Seq("-", "-o", "-")
            val (outputStream, errorStream, exitCode) = executeSpirvCmd(shaderCode, cmd)

            if (exitCode == 0) {
              logger.debug("SPIRV-Tools Optimizer succeeded.")
              Some(toDirectBuffer(ByteBuffer.wrap(outputStream.toByteArray)))
            } else {
              throw SpirvOptimizationError(s"SPIRV-Tools Optimizer failed with exit code $exitCode.\n${errorStream.toString()}")
            }
        }
    }
  }

  private def toDirectBuffer(buf: ByteBuffer): ByteBuffer = {
    val direct = ByteBuffer.allocateDirect(buf.remaining())
    direct.put(buf)
    direct.flip()
    direct
  }

  override protected def createError(message: String): SpirvOptimizationError =
    SpirvOptimizationError(message)

  sealed trait Optimization

  case class SpirvOptimizationError(msg: String) extends RuntimeException(msg)

  case class TargetEnv(version: String) extends ParamWithArgs("--target-env", version)

  case class Enable(settings: Param*) extends Optimization

  case object O extends FlagParam("-O")

  case object Os extends FlagParam("-Os")

  case object Disable extends Optimization
}