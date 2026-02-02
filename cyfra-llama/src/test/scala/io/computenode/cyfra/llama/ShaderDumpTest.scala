package io.computenode.cyfra.llama

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.{GProgram, GioProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirvtools.{SpirvCross, SpirvDisassembler, SpirvTool, SpirvToolsRunner, SpirvValidator}
import io.computenode.cyfra.llama.programs.f32.*
import io.computenode.cyfra.llama.programs.f16.*
import munit.FunSuite

import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}

/** Dumps GPU program shaders to SPIR-V assembly and GLSL for inspection.
  * 
  * This is useful for:
  *   - Comparing generated code with llama.cpp shaders
  *   - Verifying optimizations (Vec4 loads, unrolling, subgroup ops)
  *   - Debugging shader compilation issues
  */
class ShaderDumpTest extends FunSuite:
  
  val outputDir = Paths.get("cyfra-llama/output/shaders")
  
  override def beforeAll(): Unit =
    Files.createDirectories(outputDir)
  
  // ============ F32 Programs ============
  
  test("Dump F32 TiledMatmulVec shader"):
    val program = TiledMatmulVecProgram.forward(TiledMatmulVecProgram.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048
    ))
    dumpProgram("f32_tiled_matmul_vec", program)

  test("Dump F32 Q4KMatmulVec shader"):
    val program = Q4KMatmulVecProgram.forward(Q4KMatmulVecProgram.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048
    ))
    dumpProgram("f32_q4k_matmul_vec", program)

  test("Dump F32 Q6KMatmulVec shader"):
    val program = Q6KMatmulVecProgram.forward(Q6KMatmulVecProgram.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048
    ))
    dumpProgram("f32_q6k_matmul_vec", program)

  test("Dump F32 RMSNorm shader"):
    val program = RMSNormProgram.forward(RMSNormProgram.Sizes(
      numRows = 1,
      rowSize = 2048,
      eps = 1e-6f
    ))
    dumpProgram("f32_rmsnorm", program)

  test("Dump F32 RoPE shader"):
    val program = RoPEProgram.forward(RoPEProgram.Sizes(
      B = 1,
      T = 2,
      numHeads = 32,
      headSize = 64,
      theta = 10000f,
      startPos = 0
    ))
    dumpProgram("f32_rope", program)

  test("Dump F32 SwiGLU shader"):
    val program = SwiGLUProgram.forward(SwiGLUProgram.Sizes(5632))
    dumpProgram("f32_swiglu", program)

  test("Dump F32 ResidualAdd shader"):
    val program = ResidualAddProgram.forward(ResidualAddProgram.Sizes(2048))
    dumpProgram("f32_residual_add", program)

  test("Dump F32 Embedding shader"):
    val program = EmbeddingProgram.forward(EmbeddingProgram.Sizes(
      seqLen = 2,
      hiddenSize = 2048,
      vocabSize = 32000
    ))
    dumpProgram("f32_embedding", program)

  test("Dump F32 Q4K Matmul Layered shader"):
    val program = Q4KMatmulVecProgram.Layered.forward(Q4KMatmulVecProgram.Layered.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048,
      weightOffsetUint32 = 0,
      totalWeightUint32 = 2048 * 8 * 36  // 8 blocks per row
    ))
    dumpProgram("f32_q4k_matmul_vec_layered", program)

  test("Dump F32 Q6K Matmul Layered shader"):
    val program = Q6KMatmulVecProgram.Layered.forward(Q6KMatmulVecProgram.Layered.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048,
      weightOffsetBytes = 0,
      totalWeightBytes = 2048 * 8 * 210  // 8 blocks per row
    ))
    dumpProgram("f32_q6k_matmul_vec_layered", program)

  test("Dump F32 KV Cached Attention shader"):
    val program = KVCachedAttention.forward(KVCachedAttention.Sizes(
      B = 1,
      T = 1,
      NH = 32,
      NKV = 4,
      headSize = 64,
      startPos = 0,
      kCacheLayerOffset = 0,
      vCacheLayerOffset = 0,
      L = 1,
      maxSeqLen = 2048,
    ))
    dumpProgram("f32_kv_cached_attention", program)

  // ============ F16 Programs ============
  
  test("Dump F16 Embedding shader"):
    val program = F16EmbeddingProgram.forward(F16EmbeddingProgram.Sizes(
      seqLen = 2,
      hiddenSize = 2048,
      vocabSize = 32000
    ))
    dumpProgram("f16_embedding", program)

  test("Dump F16 RMSNorm shader"):
    val program = F16RMSNormProgram.forward(F16RMSNormProgram.Sizes(
      numRows = 1,
      rowSize = 2048,
      eps = 1e-6f
    ))
    dumpProgram("f16_rmsnorm", program)

  test("Dump F16 RoPE shader"):
    val program = F16RoPEProgram.forward(F16RoPEProgram.Sizes(
      B = 1,
      T = 2,
      numHeads = 32,
      headSize = 64,
      theta = 10000f
    ))
    dumpProgram("f16_rope", program)

  test("Dump F16 MatmulVec Hybrid shader (Vec4 weights, scalar input)"):
    val program = F16MatmulVecHybridProgram.forward(F16MatmulVecHybridProgram.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048
    ))
    dumpProgram("f16_matmul_vec_hybrid", program)

  test("Dump F16 MatmulVec Hybrid shader (Vec4 weights, Vec4 input)"):
    val program = F16MatmulVecHybridProgram.forwardVec4(F16MatmulVecHybridProgram.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048
    ))
    dumpProgram("f16_matmul_vec4_hybrid", program)

  test("Dump F16 SwiGLU shader"):
    val program = F16SwiGLUProgram.forward(F16SwiGLUProgram.Sizes(5632))
    dumpProgram("f16_swiglu", program)

  test("Dump F16 ResidualAdd shader"):
    val program = F16ResidualAddProgram.forward(F16ResidualAddProgram.Sizes(2048))
    dumpProgram("f16_residual_add", program)

  test("Dump F16 KV Cached Attention shader"):
    val program = F16KVCachedAttention.forward(F16KVCachedAttention.Sizes(
      B = 1,
      T = 1,
      NH = 32,
      NKV = 4,
      headSize = 64,
      startPos = 0,
      kCacheLayerOffset = 0,
      vCacheLayerOffset = 0,
      L = 1,
      maxSeqLen = 2048,
    ))
    dumpProgram("f16_kv_cached_attention", program)

  test("Dump F16 Output Vec4 shader"):
    val program = F16OutputVec4Program.forward(F16OutputVec4Program.Sizes(
      batchSize = 1,
      hiddenSize = 2048,
      vocabSize = 32000
    ))
    dumpProgram("f16_output_vec4", program)

  private def dumpProgram[P, L: Layout](name: String, program: GProgram[P, L]): Unit =
    program match
      case gioProgram: GioProgram[P, L] =>
        val layout = summon[Layout[L]]
        val bindings = layout.toBindings(layout.layoutRef).toList
        val shaderCode = DSLCompiler.compile(gioProgram.body(layout.layoutRef), bindings, gioProgram.workgroupSize)
        
        // Create runner with file outputs
        val runner = SpirvToolsRunner(
          validator = SpirvValidator.Enable(throwOnFail = false),
          disassembler = SpirvDisassembler.Enable(
            throwOnFail = false,
            toolOutput = SpirvTool.ToFile(outputDir.resolve(s"$name.spvasm"), hashSuffix = false)
          ),
          crossCompilation = SpirvCross.Enable(
            throwOnFail = false,
            toolOutput = SpirvTool.ToFile(outputDir.resolve(s"$name.glsl"), hashSuffix = false),
            settings = Seq(SpirvTool.Param("--vulkan-semantics"))
          ),
          originalSpirvOutput = SpirvTool.ToFile(outputDir.resolve(s"$name.spv"), hashSuffix = false)
        )
        
        runner.processShaderCodeWithSpirvTools(shaderCode)
        println(s"Dumped $name shaders to $outputDir")
      case _ =>
        println(s"Cannot dump $name - not a GioProgram")
