package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.ByteBuffer
import scala.sys.process.*

object SpirvOptimizer {

  sealed trait Param:
    def asStringParam: String

  sealed trait FlagParam(flag: String) extends Param:
    override def asStringParam: String = flag

  sealed trait ParamWithArgs(param: String, args: String*) extends Param:
    override def asStringParam: String = param + "=" + args.mkString("", " ", "")

  case object O extends FlagParam("-O")
  case object Os extends FlagParam("-Os")
  case class TargetEnv(version: String) extends ParamWithArgs("--target-env", version)

  sealed trait Optimization

  case class Enable(settings: Param*) extends Optimization

  case object Disable extends Optimization

  def getOptimizedSpirv(shaderCode: ByteBuffer, optimization: Optimization): Option[ByteBuffer] = {
    optimization match {
      case Disable => None
      case Enable(settings*) =>
        SpirvSystemUtils.getOS.flatMap { os =>
          SpirvSystemUtils.getToolExecutableFromPath(
            SpirvSystemUtils.SupportedSpirVTools.Optimizer, os)
        } match {
          case Some(executable) =>
            try {
              val cmd = Seq(executable) ++ settings.flatMap(_.asStringParam.split(" ")) ++ Seq("-", "-o", "-")
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
                    case e: Exception => logger.error("Writing to stdin failed when shader code was being optimized.", e)
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
                      logger.error("Reading from stdout failed during shader optimization.", e)
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
                      logger.error("Reading from stderr failed during shader optimization.", e)
                  } finally {
                    err.close()
                  }
                }
              )

              val process = cmd.run(processIO)
              val exitCode = process.exitValue()

              if (exitCode == 0) {
                logger.debug("SPIRV-Tools Optimizer succeeded.")
                logger.warn("SPIRV-Tools Optimizer is not guaranteed to produce shader code that has the exact same output as original.")
                logger.warn("SPIRV-Tools Optimizer is not guaranteed to produce shader code that is faster than original.")
                Some(toDirectBuffer(ByteBuffer.wrap(outputStream.toByteArray)))
              } else {
                logger.warn(s"SPIRV-Tools Optimizer failed with exit code $exitCode.")
                val errMsg = errorStream.toString()
                if (errMsg.nonEmpty) logger.warn(errMsg)
                None
              }
            } catch {
              case e: Exception =>
                logger.error("Exception during shader optimization", e)
                None
            }

          case None => None // Failed to find executable
        }
    }
  }

  private def toDirectBuffer(buf: ByteBuffer): ByteBuffer = {
    val direct = ByteBuffer.allocateDirect(buf.remaining())
    direct.put(buf)
    direct.flip()
    direct
  }
}