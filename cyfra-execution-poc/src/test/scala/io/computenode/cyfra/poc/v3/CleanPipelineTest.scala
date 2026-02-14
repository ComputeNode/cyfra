package io.computenode.cyfra.poc.v3

import scala.language.experimental.namedTuples
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*

// ==========================================================================
// PROGRAM LAYOUTS - Top level to avoid inner class reflection issues
// ==========================================================================

case class NormLayout(input: GBuffer[Float], weight: GBuffer[Float], output: GBuffer[Float])
case class MatMulLayout(input: GBuffer[Float], weight: GBuffer[Float], output: GBuffer[Float])
case class SwiGLULayout(gate: GBuffer[Float], up: GBuffer[Float], output: GBuffer[Float])
case class ResidualLayout(residual: GBuffer[Float], input: GBuffer[Float], output: GBuffer[Float])
case class SampleLayout(logits: GBuffer[Float], output: GBuffer[Int])

given Layout[NormLayout] = Layout.derived
given Layout[MatMulLayout] = Layout.derived
given Layout[SwiGLULayout] = Layout.derived
given Layout[ResidualLayout] = Layout.derived
given Layout[SampleLayout] = Layout.derived

/** Clean pipeline example demonstrating the desired API. */
class CleanPipelineTest extends munit.FunSuite:

  // ==========================================================================
  // PROGRAM DEFINITIONS
  // ==========================================================================

  object RMSNormProgram:
    case class Sizes(numRows: Int, rowSize: Int, eps: Float)

    def forward(sizes: Sizes): GExecution[Sizes, (GBuffer[Float], GBuffer[Float]), GBuffer[Float]] =
      GProgram[Sizes, NormLayout](
        programName = "RMSNorm",
        programParams = sizes,
        workgroup = (512, 1, 1),
        dispatchFn = (_, s) => ((s.numRows + 255) / 256, 1, 1),
      )(layout => NormLayout(layout.input, layout.weight, GBuffer.external[Float]("normed", sizes.numRows * sizes.rowSize)))
        .contramap((in: (GBuffer[Float], GBuffer[Float])) => NormLayout(in._1, in._2, GBuffer.external[Float]("_", 0)))
        .map(_.output)

  object MatMulProgram:
    case class Sizes(batchSize: Int, inFeatures: Int, outFeatures: Int)

    def forward(sizes: Sizes): GExecution[Sizes, (GBuffer[Float], GBuffer[Float]), GBuffer[Float]] =
      GProgram[Sizes, MatMulLayout](
        programName = "MatMul",
        programParams = sizes,
        workgroup = (256, 1, 1),
        dispatchFn = (_, s) => ((s.batchSize * s.outFeatures + 255) / 256, 1, 1),
      )(layout => MatMulLayout(layout.input, layout.weight, GBuffer.external[Float]("matmul_out", sizes.batchSize * sizes.outFeatures)))
        .contramap((in: (GBuffer[Float], GBuffer[Float])) => MatMulLayout(in._1, in._2, GBuffer.external[Float]("_", 0)))
        .map(_.output)

  object SwiGLUProgram:
    case class Sizes(numElements: Int)

    def forward(sizes: Sizes): GExecution[Sizes, (GBuffer[Float], GBuffer[Float]), GBuffer[Float]] =
      GProgram[Sizes, SwiGLULayout](
        programName = "SwiGLU",
        programParams = sizes,
        workgroup = (256, 1, 1),
        dispatchFn = (_, s) => ((s.numElements + 255) / 256, 1, 1),
      )(layout => SwiGLULayout(layout.gate, layout.up, GBuffer.external[Float]("swiglu_out", sizes.numElements)))
        .contramap((in: (GBuffer[Float], GBuffer[Float])) => SwiGLULayout(in._1, in._2, GBuffer.external[Float]("_", 0)))
        .map(_.output)

  object ResidualAddProgram:
    case class Sizes(numElements: Int)

    def forward(sizes: Sizes): GExecution[Sizes, (GBuffer[Float], GBuffer[Float]), GBuffer[Float]] =
      GProgram[Sizes, ResidualLayout](
        programName = "ResidualAdd",
        programParams = sizes,
        workgroup = (256, 1, 1),
        dispatchFn = (_, s) => ((s.numElements + 255) / 256, 1, 1),
      )(layout => ResidualLayout(layout.residual, layout.input, GBuffer.external[Float]("residual_out", sizes.numElements)))
        .contramap((in: (GBuffer[Float], GBuffer[Float])) => ResidualLayout(in._1, in._2, GBuffer.external[Float]("_", 0)))
        .map(_.output)

  object TopPSampleProgram:
    case class Sizes(vocabSize: Int)

    def forward(sizes: Sizes): GExecution[Sizes, GBuffer[Float], GBuffer[Int]] =
      GProgram[Sizes, SampleLayout](
        programName = "TopPSample",
        programParams = sizes,
        workgroup = (256, 1, 1),
        dispatchFn = (_, _) => (1, 1, 1),
      )(layout => SampleLayout(layout.logits, GBuffer.external[Int]("sampled_token", 1)))
        .contramap((in: GBuffer[Float]) => SampleLayout(in, GBuffer.external[Int]("_", 0)))
        .map(_.output)

  // ==========================================================================
  // EXTERNAL INPUTS
  // ==========================================================================

  case class ExternalInputs(
    hidden: GBuffer[Float],
    attnNormWeight: GBuffer[Float],
    ffnNormWeight: GBuffer[Float],
    gateWeight: GBuffer[Float],
    upWeight: GBuffer[Float],
    downWeight: GBuffer[Float],
  )

  // ==========================================================================
  // THE CLEAN PIPELINE - Pure DAG building, no side effects
  // ==========================================================================

  test("transformer layer - pure DAG building"):
    given runtime: CyfraRuntime = MockCyfraRuntime()

    val B = 1
    val T = 4
    val C = 512
    val FFN = 2048
    val V = 32000

    val attnNormSizes = RMSNormProgram.Sizes(B * T, C, 1e-5f)
    val ffnNormSizes = RMSNormProgram.Sizes(B * T, C, 1e-5f)
    val gateMatmulSizes = MatMulProgram.Sizes(B * T, C, FFN)
    val upMatmulSizes = MatMulProgram.Sizes(B * T, C, FFN)
    val downMatmulSizes = MatMulProgram.Sizes(B * T, FFN, C)
    val swiGluSizes = SwiGLUProgram.Sizes(B * T * FFN)
    val residualSizes = ResidualAddProgram.Sizes(B * T * C)
    val topSampleSizes = TopPSampleProgram.Sizes(V)

    println("\n========== PURE DAG BUILDING ==========\n")

    val ext = ExternalInputs(
      hidden = GBuffer.external[Float]("hidden", B * T * C),
      attnNormWeight = GBuffer.external[Float]("attn_norm_w", C),
      ffnNormWeight = GBuffer.external[Float]("ffn_norm_w", C),
      gateWeight = GBuffer.external[Float]("gate_w", C * FFN),
      upWeight = GBuffer.external[Float]("up_w", C * FFN),
      downWeight = GBuffer.external[Float]("down_w", FFN * C),
    )

    // Build the DAG - PURE, no side effects!
    def buildForwardPass(ext: ExternalInputs): GBuffer[Int] =
      val attnNorm = RMSNormProgram.forward(attnNormSizes)
        .dispatch((ext.hidden, ext.attnNormWeight))

      val ffnNorm = RMSNormProgram.forward(ffnNormSizes)
        .dispatch((attnNorm, ext.ffnNormWeight))

      val gateProj = MatMulProgram.forward(gateMatmulSizes)
        .dispatch((ffnNorm, ext.gateWeight))

      val upProj = MatMulProgram.forward(upMatmulSizes)
        .dispatch((ffnNorm, ext.upWeight))

      val swiGlu = SwiGLUProgram.forward(swiGluSizes)
        .dispatch(gateProj, upProj)

      val downProj = MatMulProgram.forward(downMatmulSizes)
        .dispatch((swiGlu, ext.downWeight))

      val residualAdd = ResidualAddProgram.forward(residualSizes)
        .dispatch((ext.hidden, downProj))

      val topSample = TopPSampleProgram.forward(topSampleSizes)
        .dispatch(residualAdd)

      topSample

    val outputBuffer = buildForwardPass(ext)
    println(s"Built DAG. Output buffer: ${outputBuffer.name}")
    println(s"Output provenance: ${outputBuffer.provenance}")

    println("\n--- Materializing ---\n")
    // Materialize the layout (single buffer in this case)
    val materializedOutput = runtime.materialize(outputBuffer)
    
    // Now we can read from the materialized buffer
    assert(materializedOutput.isMaterialized)

    println("\n========================================\n")

  test("simple two-step pipeline"):
    given runtime: CyfraRuntime = MockCyfraRuntime()

    val sizes = RMSNormProgram.Sizes(128, 512, 1e-5f)
    val matmulSizes = MatMulProgram.Sizes(128, 512, 32000)

    val hidden = GBuffer.external[Float]("hidden", 128 * 512)
    val normWeight = GBuffer.external[Float]("norm_w", 512)
    val outputWeight = GBuffer.external[Float]("output_w", 512 * 32000)

    println("\n========== SIMPLE TWO-STEP ==========\n")

    val normed = RMSNormProgram.forward(sizes)
      .dispatch((hidden, normWeight))

    val logits = MatMulProgram.forward(matmulSizes)
      .dispatch((normed, outputWeight))

    println(s"Normed provenance: ${normed.provenance}")
    println(s"Logits provenance: ${logits.provenance}")

    println("\n--- Materializing logits ---\n")
    // Materialize the layout containing the logits buffer
    val materializedLogits = runtime.materialize(logits)
    
    assert(materializedLogits.isMaterialized)

    println("\n=====================================\n")
