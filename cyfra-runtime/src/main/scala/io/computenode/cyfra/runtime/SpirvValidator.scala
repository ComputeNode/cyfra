package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import scala.annotation.targetName
import scala.sys.process.*
import scala.util.Try

class SpirvValidationError(msg: String) extends RuntimeException(msg)

object SpirvValidator {

  sealed trait ValidParameter {
    override def toString: String
  }

  case class MaxStructMembers(number: Int) extends ValidParameter {
    override def toString: String = s"--max-struct-members $number"
  }

  case class MaxStructDepth(number: Int) extends ValidParameter {
    override def toString: String = s"--max-struct-depth $number"
  }

  case class MaxLocalVariables(number: Int) extends ValidParameter {
    override def toString: String = s"--max-local-variables $number"
  }

  case class MaxGlobalVariables(number: Int) extends ValidParameter {
    override def toString: String = s"--max-global-variables $number"
  }

  case class MaxSwitchBranches(number: Int) extends ValidParameter {
    override def toString: String = s"--max-switch-branches $number"
  }

  case class MaxFunctionArgs(number: Int) extends ValidParameter {
    override def toString: String = s"--max-function-args $number"
  }

  case class MaxControlFlowNestingDepth(number: Int) extends ValidParameter {
    override def toString: String = s"--max-control-flow-nesting-depth $number"
  }

  case class MaxAccessChainIndexes(number: Int) extends ValidParameter {
    override def toString: String = s"--max-access-chain-indexes $number"
  }

  case class MaxIdBound(number: Int) extends ValidParameter {
    override def toString: String = s"--max-id-bound $number"
  }

  case class TargetEnv(env: String) extends ValidParameter {
    override def toString: String = s"--target-env $env"
  }

  case object RelaxLogicalPointer extends ValidParameter {
    override def toString: String = "--relax-logical-pointer"
  }

  case object RelaxBlockLayout extends ValidParameter {
    override def toString: String = "--relax-block-layout"
  }

  case object UniformBufferStandardLayout extends ValidParameter {
    override def toString: String = "--uniform-buffer-standard-layout"
  }

  case object ScalarBlockLayout extends ValidParameter {
    override def toString: String = "--scalar-block-layout"
  }

  case object WorkgroupScalarBlockLayout extends ValidParameter {
    override def toString: String = "--workgroup-scalar-block-layout"
  }

  case object SkipBlockLayout extends ValidParameter {
    override def toString: String = "--skip-block-layout"
  }

  case object RelaxStructStore extends ValidParameter {
    override def toString: String = "--relax-struct-store"
  }

  case object AllowLocalSizeId extends ValidParameter {
    override def toString: String = "--allow-localsizeid"
  }

  case object AllowOffsetTextureOperand extends ValidParameter {
    override def toString: String = "--allow-offset-texture-operand"
  }

  case object AllowVulkan32BitBitwise extends ValidParameter {
    override def toString: String = "--allow-vulkan-32-bit-bitwise"
  }

  case object BeforeHlslLegalization extends ValidParameter {
    override def toString: String = "--before-hlsl-legalization"
  }

  case object Help extends ValidParameter {
    override def toString: String = "--help"
  }

  case object Version extends ValidParameter {
    override def toString: String = "--version"
  }

  sealed trait EnableValidation

  case class Enable(settings: Option[Seq[ValidParameter]] = None) extends EnableValidation

  object Enable {
    @targetName("applyVarargs")
    def apply(settings: ValidParameter*): Enable = new Enable(Some(settings))
    def apply(): Enable = new Enable(None)
  }

  case object Disable extends EnableValidation

  def validateSpirv(shaderCode: ByteBuffer, enableValidation: EnableValidation): Unit = {
    enableValidation match {
      case Disable =>
      case Enable(settings) =>
        val tmpSpirvPath: Path = Files.createTempFile("tmp_cyfra_file", ".spv")

        try {
          SpirvSystemUtils.getOS.flatMap { os => SpirvSystemUtils.getToolExecutableFromPath(SpirvSystemUtils.SupportedSpirVTools.Validator, os) } match {
            case Some(validatorExecutable) =>
              SpirvSystemUtils.dumpSpvToFile(shaderCode, tmpSpirvPath)

              val stdout = new StringBuilder
              val stderr = new StringBuilder

              val processLogger = ProcessLogger(
                (out: String) => stdout.append(out).append("\n"),
                (err: String) => stderr.append(err).append("\n")
              )

              val process = settings match {
                case Some(settingsSeq) => Process(Seq(validatorExecutable) ++ settingsSeq.flatMap(_.toString.split(" ")) ++ Seq(tmpSpirvPath.toAbsolutePath.toString))
                case None => Process(Seq(validatorExecutable) ++ Seq(tmpSpirvPath.toAbsolutePath.toString))
              }

              val exitCode = process.!(processLogger)

              exitCode match {
                case 0 =>
                  logger.debug("SPIRV-Tools Validator succeeded.")
                  if (stdout.nonEmpty) println(stdout.toString())
                case _ =>
                  val errorMsg = s"SPIRV-Tools Validator failed with exit code $exitCode.\n" +
                    "Validation errors:\n" + stderr.toString()
                  throw new SpirvValidationError(errorMsg)
              }

            case None =>
          }
        } finally {
          Try(Files.deleteIfExists(tmpSpirvPath)).recover {
            case e: Exception =>
              logger.error(s"Failed to delete tmp spv file: ${e.getMessage}")
          }
        }
    }
  }
}
