package io.computenode.cyfra.spirvtools

import io.computenode.cyfra.spirvtools.SpirvDisassembler.Enable
import io.computenode.cyfra.spirvtools.SpirvTool.Param
import munit.FunSuite

import java.nio.ByteBuffer

class SpirvOptimizerTest extends FunSuite:

  test("SPIR-V optimization succeeded"):
    val shaderCode = SpirvTestUtils.loadShaderFromResources("original.spv")
    val optimizedShaderCode = SpirvOptimizer.optimizeSpirv(shaderCode, SpirvOptimizer.Enable(throwOnFail = true, settings = Seq(Param("-O")))) match
      case None                      => fail("Failed to optimize shader code.")
      case Some(optimizedShaderCode) => optimizedShaderCode
    val optimizedAssembly = SpirvDisassembler.disassembleSpirv(optimizedShaderCode, disassembly = Enable(throwOnFail = true))

    val referenceOptimizedShaderCode = SpirvTestUtils.loadShaderFromResources("optimized.spv")
    val referenceAssembly = SpirvDisassembler.disassembleSpirv(referenceOptimizedShaderCode, disassembly = Enable(throwOnFail = true))

    assertEquals(optimizedAssembly, referenceAssembly)
