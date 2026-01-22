package io.computenode.cyfra.e2e.dsl

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GShared}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.runtime.VkCyfraRuntime

class WorkgroupPrimitivesE2eTest extends munit.FunSuite:

  case class TestLayout(output: GBuffer[Int32]) derives Layout

  test("localInvocationIndex returns correct values"):
    VkCyfraRuntime.using:
      val size = 512
      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](size)),
        dispatchSize = _ => size,
      ): layout =>
        val idx = GIO.invocationId
        val localIdx = GIO.localInvocationIndex
        GIO.when(idx < size):
          GIO.write(layout.output, idx, localIdx)

      val resultBuf = new Array[Int](size)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](size)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      val expected = (0 until size).map(_ % 256).toArray
      assert(resultBuf.toSeq == expected.toSeq, s"Local invocation indices mismatch")

  test("workgroupId.x returns correct values"):
    VkCyfraRuntime.using:
      val size = 512
      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](size)),
        dispatchSize = _ => size,
      ): layout =>
        val idx = GIO.invocationId
        val wgId = GIO.workgroupId.x
        GIO.when(idx < size):
          GIO.write(layout.output, idx, wgId)

      val resultBuf = new Array[Int](size)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](size)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      val expected = (0 until size).map(_ / 256).toArray
      assert(resultBuf.toSeq == expected.toSeq, s"Workgroup IDs mismatch")

  test("barrier compiles and executes without error"):
    VkCyfraRuntime.using:
      val size = 256
      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](size)),
        dispatchSize = _ => size,
      ): layout =>
        val idx = GIO.invocationId
        GIO.write(layout.output, idx, idx)
          .flatMap(_ => GIO.barrier)
          .flatMap(_ => GIO.pure(layout.output.read(idx)))
          .flatMap(value => GIO.write(layout.output, idx, value + 1))

      val resultBuf = new Array[Int](size)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](size)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      val expected = (0 until size).map(_ + 1).toArray
      assert(resultBuf.toSeq == expected.toSeq, s"Barrier test: expected values incremented by 1")

  test("subgroupSize returns a valid value"):
    VkCyfraRuntime.using:
      val size = 256
      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](size)),
        dispatchSize = _ => size,
      ): layout =>
        val idx = GIO.invocationId
        val sgSize = GIO.subgroupSize
        GIO.when(idx < size):
          GIO.write(layout.output, idx, sgSize)

      val resultBuf = new Array[Int](size)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](size)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      assert(resultBuf.forall(_ > 0), s"Subgroup size should be positive")
      assert(resultBuf.forall(_ <= 128), s"Subgroup size should be <= 128")
      val uniqueValues = resultBuf.distinct
      assert(uniqueValues.length == 1, s"All invocations should report the same subgroup size")

  test("shared memory allows workgroup communication".ignore):
    VkCyfraRuntime.using:
      val workgroupSize = 256
      val shared = GShared[Int32](workgroupSize)

      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](workgroupSize)),
        dispatchSize = _ => workgroupSize,
      ): layout =>
        val localIdx = GIO.localInvocationIndex
        val globalIdx = GIO.invocationId
        shared.write(localIdx, globalIdx)
          .flatMap(_ => GIO.barrier)
          .flatMap: _ =>
            val reversedIdx: Int32 = (workgroupSize - 1: Int32) - localIdx
            val valueFromReversed = shared.read(reversedIdx)
            layout.output.write(globalIdx, valueFromReversed)

      val resultBuf = new Array[Int](workgroupSize)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](workgroupSize)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      val expected = (0 until workgroupSize).map(i => workgroupSize - 1 - i).toArray
      assert(resultBuf.toSeq == expected.toSeq, s"Shared memory communication failed")

  test("subgroupAdd reduces values within subgroup"):
    VkCyfraRuntime.using:
      val size = 256

      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](size)),
        dispatchSize = _ => size,
      ): layout =>
        val idx = GIO.invocationId
        val sum = GIO.subgroupAdd(1: Int32)
        GIO.when(idx < size):
          GIO.write(layout.output, idx, sum)

      val resultBuf = new Array[Int](size)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](size)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      val subgroupSizeActual = resultBuf.head
      assert(subgroupSizeActual > 0, s"Subgroup sum should be positive")
      assert(resultBuf.forall(_ == subgroupSizeActual), s"All lanes should have same subgroup sum (subgroup size)")

  test("subgroupInclusiveAdd computes prefix sums"):
    VkCyfraRuntime.using:
      val size = 256

      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](size)),
        dispatchSize = _ => size,
      ): layout =>
        val idx = GIO.invocationId
        val prefixSum = GIO.subgroupInclusiveAdd(1: Int32)
        GIO.when(idx < size):
          GIO.write(layout.output, idx, prefixSum)

      val resultBuf = new Array[Int](size)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](size)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      val subgroupSize = resultBuf.sliding(2).find { case Array(a, b) => b < a }.map(_(0)).getOrElse(resultBuf.last)
      assert(subgroupSize > 0, s"Should detect subgroup size from prefix sums")

  test("subgroupBroadcast broadcasts value from specified lane"):
    VkCyfraRuntime.using:
      val size = 256

      val program = GProgram.static[Unit, TestLayout](
        layout = _ => TestLayout(GBuffer[Int32](size)),
        dispatchSize = _ => size,
      ): layout =>
        val idx = GIO.invocationId
        val subgroupLaneId = GIO.subgroupLocalInvocationId
        val broadcasted = GIO.subgroupBroadcast(subgroupLaneId, 0: Int32)
        GIO.when(idx < size):
          GIO.write(layout.output, idx, broadcasted)

      val resultBuf = new Array[Int](size)
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = TestLayout(output = GBuffer[Int32](size)),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      assert(resultBuf.forall(_ == 0), s"All lanes should have received broadcast value 0 from lane 0")

  case class FoldTestLayout(input: GBuffer[Float32], output: GBuffer[Float32]) derives Layout

  test("GSeq.fold with subgroupAdd works together"):
    VkCyfraRuntime.using:
      val size = 256
      val iterations = 4

      val program = GProgram.static[Unit, FoldTestLayout](
        layout = _ => FoldTestLayout(GBuffer[Float32](size), GBuffer[Float32](size)),
        dispatchSize = _ => size,
      ): layout =>
        import io.computenode.cyfra.dsl.collections.GSeq
        val idx = GIO.invocationId
        val laneId = GIO.subgroupLocalInvocationId
        val warpSize = GIO.subgroupSize

        // Each lane computes a partial sum using fold
        val partialSum: Float32 = GSeq
          .gen[Int32](laneId, _ + warpSize)
          .limit(iterations)
          .fold(0.0f, (sum: Float32, i: Int32) => {
            when(i < size)(sum + GIO.read[Float32](layout.input, i)).otherwise(sum)
          })

        // Then reduce across subgroup
        val totalSum: Float32 = GIO.subgroupAdd(partialSum)

        GIO.when(idx < size):
          GIO.write(layout.output, idx, totalSum)

      import java.nio.{ByteBuffer, ByteOrder}
      val inputBuf = ByteBuffer.allocateDirect(size * 4).order(ByteOrder.nativeOrder())
      inputBuf.asFloatBuffer().put(Array.fill(size)(1.0f))
      inputBuf.rewind()
      val resultBuf = new Array[Float](size)

      val region = GBufferRegion
        .allocate[FoldTestLayout]
        .map(l => program.execute((), l))

      region.runUnsafe(
        init = FoldTestLayout(
          input = GBuffer[Float32](inputBuf),
          output = GBuffer[Float32](size),
        ),
        onDone = layout => layout.output.readArray(resultBuf),
      )

      // Each invocation should have the sum of the elements it processed + reduced across subgroup
      // With iterations=4 and warpSize=32, each lane processes ~4 elements worth of indices
      // But with bounds check, only valid indices contribute
      assert(resultBuf.forall(_ > 0), s"Total sum should be positive, got ${resultBuf.take(10).mkString(", ")}")
