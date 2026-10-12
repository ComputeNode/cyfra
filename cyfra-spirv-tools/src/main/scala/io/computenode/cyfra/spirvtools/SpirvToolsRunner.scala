package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvTool.{Ignore, ToFile}
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer

class SpirvToolsRunner(
  val validator: SpirvValidator.Validation = SpirvValidator.Disable,
  val optimizer: SpirvOptimizer.Optimization = SpirvOptimizer.Disable,
  val disassembler: SpirvDisassembler.Disassembly = SpirvDisassembler.Disable,
  val crossCompilation: SpirvCross.CrossCompilation = SpirvCross.Disable,
  val originalSpirvOutput: ToFile | Ignore.type = Ignore,
):

  def processShaderCodeWithSpirvTools(shaderCode: ByteBuffer): ByteBuffer =
    def runTools(code: ByteBuffer): Unit =
      SpirvDisassembler.disassembleSpirv(code, disassembler)
      SpirvCross.crossCompileSpirv(code, crossCompilation)
      SpirvValidator.validateSpirv(code, validator)

    originalSpirvOutput match
      case toFile @ SpirvTool.ToFile(_, _) =>
        toFile.write(shaderCode)
        logger.debug(s"Saved original shader code in ${toFile.filePath}.")
      case Ignore =>

    val optimized = SpirvOptimizer.optimizeSpirv(shaderCode, optimizer)
    optimized match
      case Some(optimizedCode) =>
        runTools(optimizedCode)
        optimizedCode
      case None =>
        runTools(shaderCode)
        shaderCode
