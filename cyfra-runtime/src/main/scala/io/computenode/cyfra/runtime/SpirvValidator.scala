package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.ByteBuffer
import scala.sys.process.*

class SpirvValidationError(msg: String) extends RuntimeException(msg)

object SpirvValidator {

  sealed trait Param:
    def asStringParam: String

  sealed trait FlagParam(flag: String) extends Param:
    override def asStringParam: String = flag

  sealed trait ParamWithArgs(param: String, args: String*) extends Param:
    override def asStringParam: String = param + args.mkString(" ", " ", "")

  case class TargetEnv(version: String) extends ParamWithArgs("--target-env", version)

  sealed trait Validation

  case class Enable(settings: Param*) extends Validation

  case object Disable extends Validation

  def validateSpirv(shaderCode: ByteBuffer, enableValidation: Validation): Unit = {
    enableValidation match {
      case Disable =>

      case Enable(settings*) =>
        SpirvSystemUtils.getOS.flatMap { os =>
          SpirvSystemUtils.getToolExecutableFromPath(SpirvSystemUtils.SupportedSpirVTools.Validator, os)
        } match {
          case Some(executable) =>
            val cmd = Seq(executable) ++ settings.flatMap(_.asStringParam.split(" ")) ++ Seq("-")

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
                  case e: Exception => logger.error("Writing to stdin failed when shader code was being validated.", e)
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
                    logger.error("Reading from stdout failed during shader validation.", e)
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
                    logger.error("Reading from stderr failed during shader validation.", e)
                } finally {
                  err.close()
                }
              }
            )

            val process = cmd.run(processIO)
            val exitCode = process.exitValue()

            exitCode match {
              case 0 =>
                logger.debug("SPIRV-Tools Validator succeeded.")
                if (outputStream.toString().nonEmpty) println(outputStream.toString())
              case _ =>
                throw new SpirvValidationError(s"SPIRV-Tools Validator failed with exit code $exitCode.\n" +
                  s"Validation errors:\n${errorStream.toString()}")
            }

          case None => // Failed to find executable
        }
    }
  }
}
