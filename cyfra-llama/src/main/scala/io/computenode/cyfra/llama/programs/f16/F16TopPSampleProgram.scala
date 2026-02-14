package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.{*, given}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GShared
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}

/** GPU-based Top-P (nucleus) sampling with bitonic sort.
  *
  * Uses a multi-phase approach:
  *   1. Softmax: Find max, compute exp values, sum (subgroup reductions)
  *   2. Local Top: Each thread finds its best candidate among its assigned indices
  *   3. Bitonic Sort: All 256 thread-local bests sorted in parallel
  *   4. Sample: Cumulative sum over sorted candidates, sample at threshold
  *
  * Configuration:
  *   - BLOCK_SIZE = 256 threads
  *   - 256 candidates sorted via bitonic sort
  */
object F16TopPSampleProgram:
  val WARP_SIZE = 32
  val BLOCK_SIZE = 256
  val NUM_WARPS = BLOCK_SIZE / WARP_SIZE // 8
  val LOG2_BLOCK_SIZE = 8 // log2(256)

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

  // Generate all bitonic sort steps at compile time
  // Each step is (distance, dirBlockSize) - the comparison distance and direction block size
  private val bitonicSteps: Seq[(Int, Int)] =
    for
      stage <- 0 until LOG2_BLOCK_SIZE
      subStage <- 0 to stage
    yield
      val distance = 1 << (stage - subStage)
      val dirBlockSize = 1 << (stage + 1)
      (distance, dirBlockSize)

  /** Top-p sampling with temperature scaling.
    *
    * @param sizes Contains vocabulary size
    * @return GProgram that samples a token index from logits
    */
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    given Sizes = sizes
    val vocabSize = sizes.vocabSize
    val numIterations = sizes.numIterations

    GProgram[Sizes, ProgramLayout](
      layout = _ => ProgramLayout(
        logits = GBuffer.sized[Float32](vocabSize),
        params = GUniform[SampleParams](),
        result = GBuffer.sized[Int32](1),
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
      // 256 candidates for bitonic sort
      val sharedProbs = GShared[Float32](BLOCK_SIZE)
      val sharedIndices = GShared[Int32](BLOCK_SIZE)

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
        _ <- GIO.when(tid === 0):
          sharedSum.write(0, globalSumWarp0)
        _ <- GIO.barrier
        globalSumReduced <- GIO.pure(sharedSum.read(0))
        rcpSum <- GIO.pure(1.0f / globalSumReduced)

        // ========== PHASE 2: LOCAL TOP CANDIDATE SELECTION ==========
        // Each thread finds its best candidate and writes to shared memory
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

        // Write to shared memory
        _ <- sharedProbs.write(tid, localTop.x)
        _ <- sharedIndices.write(tid, localTop.y.asInt)
        _ <- GIO.barrier

        // ========== PHASE 3: BITONIC SORT (descending by probability) ==========
        // Unrolled at compile time - 36 steps total for 256 elements
        _ <- bitonicSteps.foldLeft(GIO.pure(GStruct.Empty())) { case (acc, (distance, dirBlockSize)) =>
          acc.flatMap { _ =>
            val blockSizeVal = distance * 2
            val posInBlock: Int32 = tid.mod(blockSizeVal)

            // Only lower half of each block participates
            GIO.when(posInBlock < distance):
              val i: Int32 = tid
              val j: Int32 = tid + distance

              for
                probI <- GIO.pure(sharedProbs.read(i))
                probJ <- GIO.pure(sharedProbs.read(j))
                idxI <- GIO.pure(sharedIndices.read(i))
                idxJ <- GIO.pure(sharedIndices.read(j))

                // Determine sort direction: descending in first half of direction block
                ascending <- GIO.pure((tid / dirBlockSize).mod(2) === 0)

                // Swap if out of order (we want descending overall, so flip logic)
                shouldSwap <- GIO.pure(
                  when(ascending)(probI < probJ).otherwise(probI > probJ)
                )

                _ <- GIO.when(shouldSwap):
                  for
                    _ <- sharedProbs.write(i, probJ)
                    _ <- sharedProbs.write(j, probI)
                    _ <- sharedIndices.write(i, idxJ)
                    _ <- sharedIndices.write(j, idxI)
                  yield GStruct.Empty()
              yield GStruct.Empty()
            GIO.barrier
          }
        }

        // ========== PHASE 4: CUMULATIVE SUM + SAMPLE (single thread) ==========
        _ <- GIO.when(tid === 0) {
          // Compute cumulative sum and sample
          val threshold = randomValue * topP
          val result = GSeq.gen[Int32](0, _ + 1).limit(BLOCK_SIZE).fold(
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
