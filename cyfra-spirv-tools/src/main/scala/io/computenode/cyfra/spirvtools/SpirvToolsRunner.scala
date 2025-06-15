package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvToolsRunner.DumpOriginalSpirvToFile
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer
import java.nio.file.Path

object SpirvToolsRunner {
  sealed trait DumpOriginalSpirvToFile

  case class Yes(filePath: Path) extends DumpOriginalSpirvToFile

  case object No extends DumpOriginalSpirvToFile
}

class SpirvToolsRunner(
  val validator: SpirvValidator.Validation = SpirvValidator.Enable(),
  val optimizer: SpirvOptimizer.Optimization = SpirvOptimizer.Disable,
  val disassembler: SpirvDisassembler.Disassembly = SpirvDisassembler.Disable,
  val crossCompilation: SpirvCross.CrossCompilation = SpirvCross.Disable,
  val dumpOriginalSpirvToFile: DumpOriginalSpirvToFile = No,
) {

  def processShaderCodeWithSpirvTools(shaderCode: ByteBuffer): ByteBuffer = {
    def runTools(code: ByteBuffer): Unit = {
      SpirvDisassembler.disassembleSpirv(code, disassembler)
      SpirvCross.crossCompileSpirv(code, crossCompilation)
      SpirvValidator.validateSpirv(code, validator)
    }

    dumpOriginalSpirvToFile match {
      case SpirvToolsRunner.Yes(filePath) =>
        SpirvTool.dumpSpvToFile(shaderCode, filePath)
        logger.debug(s"Saved original shader code in $filePath.")
      case SpirvToolsRunner.No =>
    }

    val optimized = SpirvOptimizer.optimizeSpirv(shaderCode, optimizer)
    optimized match {
      case Some(optimizedCode) =>
        runTools(optimizedCode)
        optimizedCode
      case None =>
        runTools(shaderCode)
        shaderCode
    }
  }
}
