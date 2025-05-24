package io.computenode.cyfra.runtime

import io.computenode.cyfra.utility.Logger.logger

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import scala.annotation.targetName
import scala.util.Try

object SpirvOptimizer {

  sealed trait ValidParameter {
    override def toString: String
  }

  case class AmdExtToKhr() extends ValidParameter {
    override def toString: String = "--amd-ext-to-khr"
  }

  case class BeforeHlslLegalization() extends ValidParameter {
    override def toString: String = "--before-hlsl-legalization"
  }

  case class Ccp() extends ValidParameter {
    override def toString: String = "--ccp"
  }

  case class CfgCleanup() extends ValidParameter {
    override def toString: String = "--cfg-cleanup"
  }

  case class CombineAccessChains() extends ValidParameter {
    override def toString: String = "--combine-access-chains"
  }

  case class CompactIds() extends ValidParameter {
    override def toString: String = "--compact-ids"
  }

  case class ConvertLocalAccessChains() extends ValidParameter {
    override def toString: String = "--convert-local-access-chains"
  }

  case class ConvertRelaxedToHalf() extends ValidParameter {
    override def toString: String = "--convert-relaxed-to-half"
  }

  case class ConvertToSampledImage(descriptors: String) extends ValidParameter {
    override def toString: String = s"""--convert-to-sampled-image "$descriptors""""
  }

  case class CopyPropagateArrays() extends ValidParameter {
    override def toString: String = "--copy-propagate-arrays"
  }

  case class ReplaceDescArrayAccessUsingVarIndex() extends ValidParameter {
    override def toString: String = "--replace-desc-array-access-using-var-index"
  }

  case class SpreadVolatileSemantics() extends ValidParameter {
    override def toString: String = "--spread-volatile-semantics"
  }

  case class DescriptorScalarReplacement() extends ValidParameter {
    override def toString: String = "--descriptor-scalar-replacement"
  }

  case class DescriptorCompositeScalarReplacement() extends ValidParameter {
    override def toString: String = "--descriptor-composite-scalar-replacement"
  }

  case class DescriptorArrayScalarReplacement() extends ValidParameter {
    override def toString: String = "--descriptor-array-scalar-replacement"
  }

  case class EliminateDeadBranches() extends ValidParameter {
    override def toString: String = "--eliminate-dead-branches"
  }

  case class EliminateDeadCodeAggressive() extends ValidParameter {
    override def toString: String = "--eliminate-dead-code-aggressive"
  }

  case class EliminateDeadConst() extends ValidParameter {
    override def toString: String = "--eliminate-dead-const"
  }

  case class EliminateDeadFunctions() extends ValidParameter {
    override def toString: String = "--eliminate-dead-functions"
  }

  case class EliminateDeadInserts() extends ValidParameter {
    override def toString: String = "--eliminate-dead-inserts"
  }

  case class EliminateDeadInputComponents() extends ValidParameter {
    override def toString: String = "--eliminate-dead-input-components"
  }

  case class EliminateDeadVariables() extends ValidParameter {
    override def toString: String = "--eliminate-dead-variables"
  }

  case class EliminateInsertExtract() extends ValidParameter {
    override def toString: String = "--eliminate-insert-extract"
  }

  case class EliminateLocalMultiStore() extends ValidParameter {
    override def toString: String = "--eliminate-local-multi-store"
  }

  case class EliminateLocalSingleBlock() extends ValidParameter {
    override def toString: String = "--eliminate-local-single-block"
  }

  case class EliminateLocalSingleStore() extends ValidParameter {
    override def toString: String = "--eliminate-local-single-store"
  }

  case class FixFuncCallParam() extends ValidParameter {
    override def toString: String = "--fix-func-call-param"
  }

  case class FlattenDecorations() extends ValidParameter {
    override def toString: String = "--flatten-decorations"
  }

  case class FoldSpecConstOpComposite() extends ValidParameter {
    override def toString: String = "--fold-spec-const-op-composite"
  }

  case class FreezeSpecConst() extends ValidParameter {
    override def toString: String = "--freeze-spec-const"
  }

  case class GraphicsRobustAccess() extends ValidParameter {
    override def toString: String = "--graphics-robust-access"
  }

  case class IfConversion() extends ValidParameter {
    override def toString: String = "--if-conversion"
  }

  case class InlineEntryPointsExhaustive() extends ValidParameter {
    override def toString: String = "--inline-entry-points-exhaustive"
  }

  case class LegalizeHlsl() extends ValidParameter {
    override def toString: String = "--legalize-hlsl"
  }

  case class LocalRedundancyElimination() extends ValidParameter {
    override def toString: String = "--local-redundancy-elimination"
  }

  case class LoopFission() extends ValidParameter {
    override def toString: String = "--loop-fission"
  }

  case class LoopFusion() extends ValidParameter {
    override def toString: String = "--loop-fusion"
  }

  case class LoopInvariantCodeMotion() extends ValidParameter {
    override def toString: String = "--loop-invariant-code-motion"
  }

  case class LoopUnroll() extends ValidParameter {
    override def toString: String = "--loop-unroll"
  }

  case class LoopUnrollPartial() extends ValidParameter {
    override def toString: String = "--loop-unroll-partial"
  }

  case class LoopPeeling() extends ValidParameter {
    override def toString: String = "--loop-peeling"
  }

  case class LoopPeelingThreshold() extends ValidParameter {
    override def toString: String = "--loop-peeling-threshold"
  }

  case class MaxIdBound(n: Int) extends ValidParameter {
    override def toString: String = s"--max-id-bound=$n"
  }

  case class MergeBlocks() extends ValidParameter {
    override def toString: String = "--merge-blocks"
  }

  case class MergeReturn() extends ValidParameter {
    override def toString: String = "--merge-return"
  }

  case class ModifyMaximalReconvergence(action: String) extends ValidParameter {
    override def toString: String = s"--modify-maximal-reconvergence=$action"
  }

  case class LoopUnswitch() extends ValidParameter {
    override def toString: String = "--loop-unswitch"
  }

  case class O() extends ValidParameter {
    override def toString: String = "-O"
  }

  case class Os() extends ValidParameter {
    override def toString: String = "-Os"
  }

  case class OConfig(file: String) extends ValidParameter {
    override def toString: String = s"--Oconfig=$file"
  }

  case class PreserveBindings() extends ValidParameter {
    override def toString: String = "--preserve-bindings"
  }

  case class PreserveInterface() extends ValidParameter {
    override def toString: String = "--preserve-interface"
  }

  case class PreserveSpecConstants() extends ValidParameter {
    override def toString: String = "--preserve-spec-constants"
  }

  case class PrintAll() extends ValidParameter {
    override def toString: String = "--print-all"
  }

  case class PrivateToLocal() extends ValidParameter {
    override def toString: String = "--private-to-local"
  }

  case class ReduceLoadSize(threshold: Option[Int] = None) extends ValidParameter {
    override def toString: String = threshold match {
      case Some(t) => s"--reduce-load-size=$t"
      case None => "--reduce-load-size"
    }
  }

  case class RedundancyElimination() extends ValidParameter {
    override def toString: String = "--redundancy-elimination"
  }

  case class RelaxBlockLayout() extends ValidParameter {
    override def toString: String = "--relax-block-layout"
  }

  case class RelaxFloatOps() extends ValidParameter {
    override def toString: String = "--relax-float-ops"
  }

  case class RelaxLogicalPointer() extends ValidParameter {
    override def toString: String = "--relax-logical-pointer"
  }

  case class RelaxStructStore() extends ValidParameter {
    override def toString: String = "--relax-struct-store"
  }

  case class RemoveDuplicates() extends ValidParameter {
    override def toString: String = "--remove-duplicates"
  }

  case class RemoveUnusedInterfaceVariables() extends ValidParameter {
    override def toString: String = "--remove-unused-interface-variables"
  }

  case class ReplaceInvalidOpcode() extends ValidParameter {
    override def toString: String = "--replace-invalid-opcode"
  }

  case class ResolveBindingConflicts() extends ValidParameter {
    override def toString: String = "--resolve-binding-conflicts"
  }

  case class SsaRewrite() extends ValidParameter {
    override def toString: String = "--ssa-rewrite"
  }

  case class ScalarBlockLayout() extends ValidParameter {
    override def toString: String = "--scalar-block-layout"
  }

  case class ScalarReplacement(n: Option[Int] = None) extends ValidParameter {
    override def toString: String = n match {
      case Some(num) => s"--scalar-replacement=$num"
      case None => "--scalar-replacement"
    }
  }

  case class SetSpecConstDefaultValue(specDefaults: String) extends ValidParameter {
    override def toString: String = s"""--set-spec-const-default-value "$specDefaults""""
  }

  case class SimplifyInstructions() extends ValidParameter {
    override def toString: String = "--simplify-instructions"
  }

  case class SkipBlockLayout() extends ValidParameter {
    override def toString: String = "--skip-block-layout"
  }

  case class SkipValidation() extends ValidParameter {
    override def toString: String = "--skip-validation"
  }

  case class SplitCombinedImageSampler() extends ValidParameter {
    override def toString: String = "--split-combined-image-sampler"
  }

  case class StrengthReduction() extends ValidParameter {
    override def toString: String = "--strength-reduction"
  }

  case class StripDebug() extends ValidParameter {
    override def toString: String = "--strip-debug"
  }

  case class StripNonsemantic() extends ValidParameter {
    override def toString: String = "--strip-nonsemantic"
  }

  case class StripReflect() extends ValidParameter {
    override def toString: String = "--strip-reflect"
  }

  case class StructPacking(nameRule: String) extends ValidParameter {
    override def toString: String = s"--struct-packing=$nameRule"
  }

  case class SwitchDescriptorSet(fromTo: String) extends ValidParameter {
    override def toString: String = s"--switch-descriptorset=$fromTo"
  }

  case class TargetEnv(env: String) extends ValidParameter {
    override def toString: String = s"--target-env=$env"
  }

  case class TimeReport() extends ValidParameter {
    override def toString: String = "--time-report"
  }

  case class TrimCapabilities() extends ValidParameter {
    override def toString: String = "--trim-capabilities"
  }

  case class UpgradeMemoryModel() extends ValidParameter {
    override def toString: String = "--upgrade-memory-model"
  }

  case class VectorDce() extends ValidParameter {
    override def toString: String = "--vector-dce"
  }

  case class Workaround1209() extends ValidParameter {
    override def toString: String = "--workaround-1209"
  }

  case class WorkgroupScalarBlockLayout() extends ValidParameter {
    override def toString: String = "--workgroup-scalar-block-layout"
  }

  case class WrapOpkill() extends ValidParameter {
    override def toString: String = "--wrap-opkill"
  }

  case class UnifyConst() extends ValidParameter {
    override def toString: String = "--unify-const"
  }

  case class ValidateAfterAll() extends ValidParameter {
    override def toString: String = "--validate-after-all"
  }

  case class Help() extends ValidParameter {
    override def toString: String = "--help"
  }

  case class Version() extends ValidParameter {
    override def toString: String = "--version"
  }

  sealed trait EnableOptimization

  case class Enable(settings: Seq[ValidParameter]) extends EnableOptimization
  
  object Enable {
    @targetName("applyVarargs")
    def apply(settings: ValidParameter*): Enable = new Enable(settings)
  }

  case object Disable extends EnableOptimization

  def getOptimizedSpirv(shaderCode: ByteBuffer, enableOptimization: EnableOptimization): Option[ByteBuffer] = {
    enableOptimization match {
      case Disable => None
      case Enable(settings) =>
        val tmpSpirvPath: Path = Files.createTempFile("tmp_cyfra_file", ".spv")

        try {
          SpirvSystemUtils.getOS.flatMap { os =>
            SpirvSystemUtils.getToolExecutableFromPath(
              SpirvSystemUtils.SupportedSpirVTools.Optimizer, os)
          } match {
            case Some(executable) =>
              SpirvSystemUtils.dumpSpvToFile(shaderCode, tmpSpirvPath)

              val processBuilder = new ProcessBuilder(
                Seq(executable) ++ settings.flatMap(_.toString.split(" ")) ++ Seq(tmpSpirvPath.toAbsolutePath.toString, "-o", "-") *
              )
              val process = processBuilder.start()

              val outputBytes = try {
                val in: InputStream = process.getInputStream
                val out = new ByteArrayOutputStream()
                val buffer = new Array[Byte](1024)
                var bytesRead = in.read(buffer)
                while (bytesRead != -1) {
                  out.write(buffer, 0, bytesRead)
                  bytesRead = in.read(buffer)
                }
                out.toByteArray
              } finally {
                process.getInputStream.close()
              }

              val exitCode = process.waitFor()
              if (exitCode != 0) {
                logger.warn(s"SPIR-V optimizer failed with exit code $exitCode.")

                val err = new String(process.getErrorStream.readAllBytes(), "UTF-8")
                if (err.nonEmpty) logger.warn(s"$err")

                logger.warn("Falling back to original shader code.")
                None
              } else {
                logger.debug(s"Optimization of shader code was successful.")
                Some(toDirectBuffer(ByteBuffer.wrap(outputBytes)))
              }
            case None =>
              logger.warn(s"Optimization of shader code failed. Falling back to original shader code.")
              None
          }

        } finally {
          Try(Files.deleteIfExists(tmpSpirvPath)).recover {
            case e: Exception =>
              logger.error(s"Failed to delete ${tmpSpirvPath.toAbsolutePath}: ${e.getMessage}")
          }
        }
    }
  }

  private def toDirectBuffer(buffer: ByteBuffer): ByteBuffer = {
    val direct = ByteBuffer.allocateDirect(buffer.remaining())
    direct.order(buffer.order())
    direct.put(buffer)
    direct.flip()
    direct
  }
}