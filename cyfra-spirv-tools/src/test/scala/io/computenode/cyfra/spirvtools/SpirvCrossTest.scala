package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvCross.Enable
import munit.FunSuite

class SpirvCrossTest extends FunSuite {

  test("SPIR-V cross compilation succeeded") {
    val shaderCode = SpirvTestUtils.loadShaderFromResources("optimized.spv")
    val glslShader = SpirvCross.crossCompileSpirv(shaderCode, crossCompilation = Enable(throwOnFail = true)) match {
      case None => fail("Failed to disassemble shader.")
      case Some(assembly) => assembly
    }

    val referenceGlsl = SpirvTestUtils.loadResourceAsString("optimized.glsl")

    assertEquals(glslShader, referenceGlsl)
  }
}



