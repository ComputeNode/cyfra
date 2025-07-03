package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvValidator.Enable
import munit.FunSuite

class SpirvValidatorTest extends FunSuite:

  test("SPIR-V validation succeeded"):
    val shaderCode = SpirvTestUtils.loadShaderFromResources("optimized.spv")

    try
      SpirvValidator.validateSpirv(shaderCode, validation = Enable(throwOnFail = true))
      assert(true)
    catch
      case e: Throwable =>
        fail(s"Validation unexpectedly failed: ${e.getMessage}")

  test("SPIR-V validation fail"):
    val shaderCode = SpirvTestUtils.loadShaderFromResources("optimized.spv")
    val corruptedShaderCode = SpirvTestUtils.corruptMagicNumber(shaderCode)

    try
      SpirvValidator.validateSpirv(corruptedShaderCode, validation = Enable(throwOnFail = true))
      fail(s"Validation was supposed to fail.")
    catch
      case e: Throwable =>
        val result = e.getMessage
        assertEquals(result, "SPIR-V validation failed with exit code 1.\nValidation errors:\nerror: line 0: Invalid SPIR-V magic number.\n")
