package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GShared
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}

/** GPU-based Top-P (nucleus) sampling.
  *
  * Uses a multi-phase approach:
  *   1. Softmax: Find max, compute exp values, sum (subgroup reductions)
  *   2. Local Top: Each thread finds its best candidate
  *   3. Warp reduction: Each warp finds its best candidate (8 total)
  *   4. Sort: Simple bubble sort on 8 candidates
  *   5. Sample: Cumulative sum over sorted candidates, sample at threshold
  *
  * For peaked LLM distributions, 8 warp-best candidates typically cover 90%+ probability.
  * Falls back to argmax if sampling fails.
  *
  * Configuration:
  *   - BLOCK_SIZE = 256 threads (8 warps)
  *   - NUM_WARPS = 8 candidates for sampling
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
  ) extends GStruct[SampleParams]

  object SampleParams:
    given GStructSchema[SampleParams] = GStructSchema.derived

  case class ProgramLayout(
    logits: GBuffer[Float32],
    params: GUniform[SampleParams],
    result: GBuffer[Int32], // Output: single sampled token index
  ) derives Layout

  /** Top-p sampling with temperature scaling.
    *
    * @param sizes Contains vocabulary size
    * @return GProgram that samples a token index from logits
    */
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val vocabSize = sizes.vocabSize
    val numIterations = sizes.numIterations

    GProgram[Sizes, ProgramLayout](
      layout = _ => ProgramLayout(
        logits = GBuffer[Float32](vocabSize),
        params = GUniform[SampleParams](),
        result = GBuffer[Int32](1),
      ),
      dispatch = (_, _) => StaticDispatch((1, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val laneId: Int32 = tid.mod(WARP_SIZE)
      val warpId: Int32 = tid / WARP_SIZE

      val runtimeParams = layout.params.read
      val temperature: Float32 = runtimeParams.temperature
      val topP: Float32 = runtimeParams.topP
      val randomValue: Float32 = runtimeParams.randomValue

      // Shared memory for reductions
      val sharedMax = GShared[Float32](NUM_WARPS)
      val sharedSum = GShared[Float32](NUM_WARPS)
      // Store warp-best candidates: 8 probs and 8 indices
      val sharedProbs = GShared[Float32](NUM_WARPS)
      val sharedIndices = GShared[Int32](NUM_WARPS)

      for
        // ========== PHASE 1: SOFTMAX ==========
        // Step 1a: Each thread finds local max
        localMax <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).fold(-1e10f, (maxVal: Float32, idx: Int32) =>
            when(idx < vocabSize)(
              max(maxVal, GIO.read[Float32](layout.logits, idx))
            ).otherwise(maxVal)
          )
        }

        // Step 1b: Warp reduction for max
        warpMax <- GIO.pure(GIO.subgroupMax(localMax))
        _ <- GIO.when(laneId === 0):
          sharedMax.write(warpId, warpMax)
        _ <- GIO.barrier

        // Step 1c: Final max reduction (first warp reads all, reduces)
        globalMax <- GIO.pure {
          when(tid < NUM_WARPS)(sharedMax.read(tid)).otherwise(-1e10f)
        }
        globalMaxWarp0 <- GIO.pure(GIO.subgroupMax(globalMax))
        // Broadcast global max to all threads via shared memory
        _ <- GIO.when(tid === 0):
          sharedMax.write(0, globalMaxWarp0)
        _ <- GIO.barrier
        globalMaxReduced <- GIO.pure(sharedMax.read(0))

        // Step 1d: Temperature scaling factor
        invTemp <- GIO.pure(when(temperature > 0.0001f)(1.0f / temperature).otherwise(1.0f))

        // Step 1e: Each thread computes local exp sum
        localSum <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).fold(0.0f, (sumVal: Float32, idx: Int32) =>
            when(idx < vocabSize) {
              val logit: Float32 = GIO.read[Float32](layout.logits, idx)
              val expVal: Float32 = exp((logit - globalMaxReduced) * invTemp)
              sumVal + expVal
            }.otherwise(sumVal)
          )
        }

        // Step 1f: Warp reduction for sum
        warpSum <- GIO.pure(GIO.subgroupAdd(localSum))
        _ <- GIO.when(laneId === 0):
          sharedSum.write(warpId, warpSum)
        _ <- GIO.barrier

        // Step 1g: Final sum reduction
        globalSum <- GIO.pure {
          when(tid < NUM_WARPS)(sharedSum.read(tid)).otherwise(0.0f)
        }
        globalSumWarp0 <- GIO.pure(GIO.subgroupAdd(globalSum) + 1e-10f)
        // Broadcast global sum to all threads via shared memory
        _ <- GIO.when(tid === 0):
          sharedSum.write(0, globalSumWarp0)
        _ <- GIO.barrier
        globalSumReduced <- GIO.pure(sharedSum.read(0))
        rcpSum <- GIO.pure(1.0f / globalSumReduced)

        // ========== PHASE 2: LOCAL TOP CANDIDATE SELECTION ==========
        // Each thread finds its best candidate (prob, idx) stored as Vec2
        localTop <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).fold(
            vec2(-1e10f, -1.0f), // (maxProb, maxIdx)
            (state: Vec2[Float32], idx: Int32) =>
              when(idx < vocabSize) {
                val logit: Float32 = GIO.read[Float32](layout.logits, idx)
                val prob: Float32 = exp((logit - globalMaxReduced) * invTemp) * rcpSum
                when(prob > state.x)(vec2(prob, idx.asFloat)).otherwise(state)
              }.otherwise(state)
          )
        }

        // ========== PHASE 3: WARP-LEVEL REDUCTION TO GET TOP 8 ==========
        // Each warp finds its best candidate using subgroup reductions
        warpBestProb <- GIO.pure(GIO.subgroupMax(localTop.x))

        // Tolerance-based comparison to handle floating-point precision
        isWarpBest <- GIO.pure(localTop.x >= warpBestProb - 1e-7f)

        // For threads with best prob, use their index; others use large value
        // subgroupMin will select the lowest index among ties
        warpBestIdx <- GIO.pure(when(isWarpBest)(localTop.y).otherwise(1e10f))
        warpWinnerIdx <- GIO.pure(GIO.subgroupMin(warpBestIdx))

        // Lane 0 of each warp writes the result
        _ <- GIO.when(laneId === 0):
          for
            _ <- sharedProbs.write(warpId, warpBestProb)
            _ <- sharedIndices.write(warpId, warpWinnerIdx.asInt)
          yield GStruct.Empty()
        _ <- GIO.barrier

        // ========== PHASE 4: SORT 8 CANDIDATES (single thread) ==========
        // Simple bubble sort for 8 elements - very fast
        _ <- GIO.when(tid === 0) {
          GIO.repeat(NUM_WARPS): _ =>
            GIO.repeat(NUM_WARPS - 1): j =>
              val jIdx: Int32 = j
              val jIdxPlus1: Int32 = jIdx + 1
              // Read all values BEFORE any writes to avoid the swap bug
              for
                prob0 <- GIO.pure(sharedProbs.read(jIdx))
                prob1 <- GIO.pure(sharedProbs.read(jIdxPlus1))
                idx0 <- GIO.pure(sharedIndices.read(jIdx))
                idx1 <- GIO.pure(sharedIndices.read(jIdxPlus1))
                // Swap if out of order (descending)
                _ <- GIO.when(prob0 < prob1):
                  for
                    _ <- sharedProbs.write(jIdx, prob1)
                    _ <- sharedProbs.write(jIdxPlus1, prob0)
                    _ <- sharedIndices.write(jIdx, idx1)
                    _ <- sharedIndices.write(jIdxPlus1, idx0)
                  yield GStruct.Empty()
              yield GStruct.Empty()
        }
        _ <- GIO.barrier

        // ========== PHASE 5: CUMULATIVE SUM + SAMPLE ==========
        _ <- GIO.when(tid === 0) {
          // Compute cumulative sum and sample
          val threshold = randomValue * topP
          val result = GSeq.gen[Int32](0, _ + 1).limit(NUM_WARPS).fold(
            vec2(0.0f, -1.0f), // (cumSum, sampledIdx)
            (state: Vec2[Float32], i: Int32) =>
              val cumSum = state.x
              val sampledIdx = state.y
              val prob = sharedProbs.read(i)
              val idx = sharedIndices.read(i)
              val newCumSum = cumSum + prob
              // Only set sampledIdx if not already set AND we've exceeded threshold
              val newSampledIdx = when(sampledIdx < 0.0f && newCumSum >= threshold)(idx.asFloat).otherwise(sampledIdx)
              vec2(newCumSum, newSampledIdx)
          )
          // If sampling didn't find a result, use argmax (position 0 after sort)
          val finalIdx = when(result.y >= 0.0f)(result.y.asInt).otherwise(sharedIndices.read(0))
          GIO.write[Int32](layout.result, 0, finalIdx)
        }
      yield GStruct.Empty()
