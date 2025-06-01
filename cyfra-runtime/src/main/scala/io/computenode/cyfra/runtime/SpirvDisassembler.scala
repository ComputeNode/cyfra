package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

object SpirvDisassembler extends SpirvTool {

  override type SpirvError = SpirvDisassemblerError
  protected override val toolName: SupportedSpirVTools = SupportedSpirVTools.Disassembler

  def getDisassembledSpirv(shaderCode: ByteBuffer, options: Param*): Option[String] = {
    getOS.flatMap { os =>
      getToolExecutableFromPath(
        toolName, os)
    } match {
      case None =>
        logger.warn("Shader code will not be disassembled.")
        None
      case Some(executable) =>
        val cmd = Seq(executable) ++ options.flatMap(_.asStringParam.split(" ")) ++ Seq("-")
        val (outputStream, errorStream, exitCode) = executeSpirvCmd(shaderCode, cmd)

        if (exitCode == 0) {
          logger.debug("SPIRV-Tools Disassembler succeeded.")
          Some(outputStream.toString)
        } else {
          throw SpirvDisassemblerError(s"SPIRV-Tools Disassembler failed with exit code $exitCode. ${errorStream.toString}")
        }
    }
  }

  override protected def createError(message: String): SpirvDisassemblerError =
    SpirvDisassemblerError(message)

  case class SpirvDisassemblerError(msg: String) extends RuntimeException(msg)

  case object NoIndent extends FlagParam("--no-indent")

  case object NoHeader extends FlagParam("--no-header")

  case object RawId extends FlagParam("--raw-id")

  case object NestedIndent extends FlagParam("--nested-indent")

  case object ReorderBlocks extends FlagParam("--reorder-blocks")

  case object Offsets extends FlagParam("--offsets")

  case object Comment extends FlagParam("--comment")
}
