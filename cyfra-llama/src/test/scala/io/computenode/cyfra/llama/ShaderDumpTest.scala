package io.computenode.cyfra.llama

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.{GProgram, GioProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirvtools.{SpirvCross, SpirvDisassembler, SpirvTool, SpirvToolsRunner, SpirvValidator}
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

  // ============ F16 Programs ============

  test("Dump F16 Embedding shader"):
    val program = F16EmbeddingProgram.forward(F16EmbeddingProgram.Sizes(
      seqLen = 2,
      hiddenSize = 2048,
      vocabSize = 32000,
    ))
    dumpProgram("f16_embedding", program)

  test("Dump F16 RMSNorm shader"):
    val program = F16RMSNormProgram.forward(F16RMSNormProgram.Sizes(
      numRows = 1,
      rowSize = 2048,
      eps = 1e-6f,
    ))
    dumpProgram("f16_rmsnorm", program)

  test("Dump F16 Fused RoPE shader"):
    val program = F16FusedRoPEProgram.forward(F16FusedRoPEProgram.Sizes(
      B = 1,
      T = 2,
      numHeadsQ = 32,
      numHeadsK = 8,
      headSize = 64,
      theta = 10000f,
    ))
    dumpProgram("f16_fused_rope", program)

  test("Dump F16 MatmulVec Hybrid shader (Vec4 weights, scalar input)"):
    val program = F16MatmulVecHybridProgram.forward(F16MatmulVecHybridProgram.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048,
    ))
    dumpProgram("f16_matmul_vec_hybrid", program)

  test("Dump F16 MatmulVec Hybrid shader (Vec4 weights, Vec4 input)"):
    val program = F16MatmulVecHybridProgram.forwardVec4(F16MatmulVecHybridProgram.Sizes(
      batchSize = 1,
      inFeatures = 2048,
      outFeatures = 2048,
    ))
    dumpProgram("f16_matmul_vec4_hybrid", program)

  test("Dump F16 SwiGLU shader"):
    val program = F16SwiGLUProgram.forward(F16SwiGLUProgram.Sizes(5632))
    dumpProgram("f16_swiglu", program)

  test("Dump F16 ResidualAdd shader"):
    val program = F16ResidualAddProgram.forward(F16ResidualAddProgram.Sizes(2048))
    dumpProgram("f16_residual_add", program)

  test("Dump F16 Output Vec4 shader"):
    val program = F16OutputVec4Program.forward(F16OutputVec4Program.Sizes(
      batchSize = 1,
      hiddenSize = 2048,
      vocabSize = 32000,
    ))
    dumpProgram("f16_output_vec4", program)

  // ============ Split Attention Programs ============

  test("Dump F16 Attention Scores shader"):
    val program = F16AttentionScoresProgram.forward(F16AttentionScoresProgram.Sizes(
      B = 1,
      T = 1,
      NH = 32,
      NKV = 8,
      headSize = 64,
      maxSeqLen = 2048,
      kCacheLayerOffset = 0,
      L = 16,
    ))
    dumpProgram("f16_attention_scores", program)

  test("Dump F16 Attention Softmax shader"):
    val program = F16AttentionSoftmaxProgram.forward(F16AttentionSoftmaxProgram.Sizes(
      B = 1,
      T = 1,
      NH = 32,
      maxSeqLen = 2048,
    ))
    dumpProgram("f16_attention_softmax", program)

  test("Dump F16 Attention Output shader"):
    val program = F16AttentionOutputProgram.forward(F16AttentionOutputProgram.Sizes(
      B = 1,
      T = 1,
      NH = 32,
      NKV = 8,
      headSize = 64,
      maxSeqLen = 2048,
      vCacheLayerOffset = 0,
      L = 16,
    ))
    dumpProgram("f16_attention_output", program)

  test("Dump F16 TopP Sample shader"):
    val program = F16TopPSampleProgram.forward(F16TopPSampleProgram.Sizes(
      vocabSize = 32000,
    ))
    dumpProgram("f16_top_p_sample", program)

  test("Dump F16 Fused Attention shader"):
    val program = F16FusedAttentionProgram.forward(F16FusedAttentionProgram.Sizes(
      B = 1,
      T = 1,
      NH = 32,
      NKV = 8,
      headSize = 64,
      maxSeqLen = 2048,
      kCacheLayerOffset = 0,
      vCacheLayerOffset = 0,
      L = 16,
    ))
    dumpProgram("f16_fused_attention", program)

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
            toolOutput = SpirvTool.ToFile(outputDir.resolve(s"$name.spvasm"), hashSuffix = false),
            settings = Seq(),
          ),
          crossCompilation = SpirvCross.Enable(
            throwOnFail = false,
            toolOutput = SpirvTool.ToFile(outputDir.resolve(s"$name.glsl"), hashSuffix = false),
            settings = Seq(SpirvTool.Param("--vulkan-semantics")),
          ),
          originalSpirvOutput = SpirvTool.ToFile(outputDir.resolve(s"$name.spv"), hashSuffix = false),
        )

        runner.processShaderCodeWithSpirvTools(shaderCode)
        println(s"Dumped $name shaders to $outputDir")
      case _ =>
        println(s"Cannot dump $name - not a GioProgram")
