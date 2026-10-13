package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.core.memory.GShared
import io.computenode.cyfra.dsl.direct.GIO

/** GPU-based sampling from thread-local top candidates.
  *
  * Uses a multi-phase approach:
  *   1. Softmax: Find max, compute exp values, sum (subgroup reductions)
  *   2. Local Top: Each thread finds its best candidate among its assigned indices
  *   3. Sample: Cumulative sum over candidates in lane order, sample at threshold
  *
  * Configuration:
  *   - BLOCK_SIZE = 256 threads
  *   - 256 thread-local candidates
  */
object F16TopPSampleProgram:
  val WARP_SIZE = 32
  val BLOCK_SIZE = 256
  val NUM_WARPS = BLOCK_SIZE / WARP_SIZE // 8

  case class Sizes(vocabSize: Int):
    def numIterations: Int = (vocabSize + BLOCK_SIZE - 1) / BLOCK_SIZE

  case class SampleParams(
    temperature: Float32,
    topP: Float32,
    randomValue: Float32, // Pre-generated random [0, 1)
  ) derives Value

  case class ProgramLayout(
    logits: GBuffer[GArray[Float32]],
    params: GUniform[SampleParams],
    result: GBuffer[GArray[Int32]], // Output: single sampled token index
  ) derives Layout

  /** Top-p sampling with temperature scaling.
    *
    * @param sizes
    *   Contains vocabulary size
    * @return
    *   GProgram that samples a token index from logits
    */
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val vocabSize = sizes.vocabSize
    val numIterations = sizes.numIterations

    GioProgram[Sizes, ProgramLayout](
      layout = _ => ProgramLayout(logits = GBuffer[GArray[Float32]](vocabSize), params = GUniform[SampleParams](), result = GBuffer[GArray[Int32]](1)),
      dispatch = (_, _) => StaticDispatch((1, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val laneId: Int32 = tid % WARP_SIZE
      val warpId: Int32 = tid / WARP_SIZE

      val runtimeParams = GIO.read(layout.params)
      val temperature: Float32 = runtimeParams.temperature
      val topP: Float32 = runtimeParams.topP
      val randomValue: Float32 = runtimeParams.randomValue

      // Shared memory for reductions
      val sharedMax = GShared[Float32](NUM_WARPS)
      val sharedSum = GShared[Float32](NUM_WARPS)
      // 256 thread-local candidates
      val sharedProbs = GShared[Float32](BLOCK_SIZE)
      val sharedIndices = GShared[Int32](BLOCK_SIZE)

      // ========== PHASE 1: SOFTMAX ==========
      // Step 1a: Each thread finds local max
      val localMax = GIO.foldRepeat[Float32](numIterations, -1e10f) { (iter, maxVal) =>
        val idx: Int32 = tid + iter * BLOCK_SIZE
        GIO.branch(idx < vocabSize, max(maxVal, GIO.read(layout.logits.focus(_.at(idx)))), maxVal)
      }

      // Step 1b: Warp reduction for max
      val warpMax = GIO.subgroupMax(localMax)
      GIO.when(laneId === 0):
        GIO.write(sharedMax.focus(_.at(warpId)), warpMax)
      GIO.barrier

      // Step 1c: Final max reduction (first warp reads all, reduces)
      val globalMax = GIO.branch(tid < NUM_WARPS, GIO.read(sharedMax.focus(_.at(tid))), Float32(-1e10f))
      val globalMaxWarp0 = GIO.subgroupMax(globalMax)
      GIO.when(tid === 0):
        GIO.write(sharedMax.focus(_.at(0)), globalMaxWarp0)
      GIO.barrier
      val globalMaxReduced = GIO.read(sharedMax.focus(_.at(0)))

      // Step 1d: Temperature scaling factor
      val invTemp = GIO.branch(temperature > 0.0001f, 1.0f / temperature, Float32(1.0f))

      // Step 1e: Each thread computes local exp sum
      val localSum = GIO.foldRepeat[Float32](numIterations, 0.0f) { (iter, sumVal) =>
        val idx: Int32 = tid + iter * BLOCK_SIZE
        GIO.branch(
          idx < vocabSize, {
            val logit: Float32 = GIO.read(layout.logits.focus(_.at(idx)))
            val expVal: Float32 = exp((logit - globalMaxReduced) * invTemp)
            sumVal + expVal
          },
          sumVal,
        )
      }

      // Step 1f: Warp reduction for sum
      val warpSum = GIO.subgroupAdd(localSum)
      GIO.when(laneId === 0):
        GIO.write(sharedSum.focus(_.at(warpId)), warpSum)
      GIO.barrier

      // Step 1g: Final sum reduction
      val globalSum = GIO.branch(tid < NUM_WARPS, GIO.read(sharedSum.focus(_.at(tid))), Float32(0.0f))
      val globalSumWarp0 = GIO.eval(GIO.subgroupAdd(globalSum) + 1e-10f)
      GIO.when(tid === 0):
        GIO.write(sharedSum.focus(_.at(0)), globalSumWarp0)
      GIO.barrier
      val globalSumReduced = GIO.read(sharedSum.focus(_.at(0)))
      val rcpSum = GIO.eval(1.0f / globalSumReduced)

      // ========== PHASE 2: LOCAL TOP CANDIDATE SELECTION ==========
      // Each thread finds its best candidate and writes to shared memory
      val localTop = GIO.foldRepeat[Vec2[Float32]](numIterations, Vec2(-1e10f, -1.0f)) { (iter, state) =>
        val idx: Int32 = tid + iter * BLOCK_SIZE
        GIO.branch(
          idx < vocabSize, {
            val logit: Float32 = GIO.read(layout.logits.focus(_.at(idx)))
            val prob: Float32 = exp((logit - globalMaxReduced) * invTemp) * rcpSum
            GIO.branch(prob > state.x, Vec2(prob, idx.asFloat), state)
          },
          state,
        )
      }

      // Write to shared memory
      GIO.write(sharedProbs.focus(_.at(tid)), localTop.x)
      GIO.write(sharedIndices.focus(_.at(tid)), localTop.y.asInt32)
      GIO.barrier

      // Preserve the original sampler's lane ordering: its monadic sort result was discarded.
      // Sorting these candidates would change the generated token distribution.

      // ========== PHASE 3: CUMULATIVE SUM + SAMPLE (single thread) ==========
      GIO.when(tid === 0) {
        // Compute cumulative sum and sample
        val threshold = randomValue * topP
        val result = GIO.foldRepeat[Vec2[Float32]](BLOCK_SIZE, Vec2(0.0f, -1.0f)) { (iter, state) =>
          val i: Int32 = iter
          val cumSum = state.x
          val sampledIdx = state.y
          val prob = GIO.read(sharedProbs.focus(_.at(i)))
          val idx = GIO.read(sharedIndices.focus(_.at(i)))
          val newCumSum = cumSum + prob
          // Only set sampledIdx if not already set AND we've exceeded threshold
          val newSampledIdx = GIO.branch(sampledIdx < 0.0f && newCumSum >= threshold, idx.asFloat, sampledIdx)
          Vec2(newCumSum, newSampledIdx)
        }
        // If sampling did not find a result, use the first lane's candidate
        val finalIdx = GIO.branch(result.y >= 0.0f, result.y.asInt32, GIO.read(sharedIndices.focus(_.at(0))))
        GIO.write(layout.result.focus(_.at(0)), finalIdx)
      }
