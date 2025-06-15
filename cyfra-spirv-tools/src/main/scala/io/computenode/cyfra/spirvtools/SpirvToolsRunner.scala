package io.computenode.cyfra.spirvtools

import java.nio.ByteBuffer

class SpirvToolsRunner(val validator: SpirvValidator.Validation = SpirvValidator.Enable(),
                       val optimizer: SpirvOptimizer.Optimization = SpirvOptimizer.Disable,
                       val disassembler: SpirvDisassembler.Disassembly = SpirvDisassembler.Disable,
                       val crossCompilation: SpirvCross.CrossCompilation = SpirvCross.Disable) {

  def processShaderCodeWithSpirvTools(shaderCode: ByteBuffer): ByteBuffer = {
    def runTools(code: ByteBuffer): Unit = {
      SpirvDisassembler.disassembleSpirv(code, disassembler)
      SpirvCross.crossCompileSpirv(code, crossCompilation)
      SpirvValidator.validateSpirv(code, validator)
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
