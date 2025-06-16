package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvDisassembler.Enable
import munit.FunSuite

class SpirvDisassemblerTest extends FunSuite {

  test("SPIR-V disassembly succeeded") {
    val shaderCode = SpirvTestUtils.loadShaderFromResources("optimized.spv")
    val assembly = SpirvDisassembler.disassembleSpirv(shaderCode, disassembly = Enable(throwOnFail = true)) match {
      case None           => fail("Failed to disassemble shader.")
      case Some(assembly) => assembly
    }

    val referenceAssembly = SpirvTestUtils.loadResourceAsString("optimized.spvasm")

    assertEquals(assembly, referenceAssembly)
  }
}
