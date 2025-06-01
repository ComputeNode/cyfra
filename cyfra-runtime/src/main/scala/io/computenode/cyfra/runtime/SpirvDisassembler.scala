package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.ByteBuffer
import scala.sys.process.*

class SpirvDisassemblerError(msg: String) extends RuntimeException(msg)

object SpirvDisassembler {

  sealed trait Param:
    def asStringParam: String

  sealed trait FlagParam(flag: String) extends Param:
    override def asStringParam: String = flag

  case object NoIndent extends FlagParam("--no-indent")
  case object NoHeader extends FlagParam("--no-header")
  case object RawId extends FlagParam("--raw-id")
  case object NestedIndent extends FlagParam("--nested-indent")
  case object ReorderBlocks extends FlagParam("--reorder-blocks")
  case object Offsets extends FlagParam("--offsets")
  case object Comment extends FlagParam("--comment")

  def getDisassembledSpirv(shaderCode: ByteBuffer, options: Param*): Option[String] = {
    SpirvSystemUtils.getOS.flatMap { os =>
      SpirvSystemUtils.getToolExecutableFromPath(
        SpirvSystemUtils.SupportedSpirVTools.Disassembler, os)
    } match {
      case None =>
        logger.warn("Shader code will not be disassembled.")
        None
      case Some(executable) =>
        val cmd = Seq(executable) ++ options.flatMap(_.asStringParam.split(" ")) ++ Seq("-")
        val inputBytes = {
          val arr = new Array[Byte](shaderCode.remaining())
          shaderCode.get(arr)
          shaderCode.rewind()
          arr
        }

        val inputStream = new ByteArrayInputStream(inputBytes)
        val outputStream = new ByteArrayOutputStream()
        val errorStream = new ByteArrayOutputStream()

        val processIO = new ProcessIO(
          in => {
            try {
              val buf = new Array[Byte](1024)
              var len = inputStream.read(buf)
              while (len != -1) {
                in.write(buf, 0, len)
                len = inputStream.read(buf)
              }
            } catch {
              case e: Exception => throw SpirvDisassemblerError("Writing to stdin failed when shader code was being disassembled.\n" + e)
            } finally {
              in.close()
            }
          },
          out => {
            try {
              val buf = new Array[Byte](1024)
              var len = out.read(buf)
              while (len != -1) {
                outputStream.write(buf, 0, len)
                len = out.read(buf)
              }
            } catch {
              case e: Exception =>
                throw SpirvDisassemblerError("Reading from stdout failed during shader disassembly.\n" + e)
            } finally {
              out.close()
            }
          },
          err => {
            try {
              val buf = new Array[Byte](1024)
              var len = err.read(buf)
              while (len != -1) {
                errorStream.write(buf, 0, len)
                len = err.read(buf)
              }
            } catch {
              case e: Exception =>
                throw SpirvDisassemblerError("Reading from stderr failed during shader disassembly.\n"+ e)
            } finally {
              err.close()
            }
          }
        )

        val process = cmd.run(processIO)
        val exitCode = process.exitValue()

        if (exitCode == 0) {
          logger.debug("SPIRV-Tools Disassembler succeeded.")
          Some(outputStream.toString)
        } else {
          throw SpirvDisassemblerError(s"SPIRV-Tools Disassembler failed with exit code $exitCode. ${errorStream.toString()}")
        }
    }
  }
}
