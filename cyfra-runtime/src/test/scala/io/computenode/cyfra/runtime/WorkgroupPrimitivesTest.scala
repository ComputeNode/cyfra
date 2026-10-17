package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{GProgram, Layout}
import io.computenode.cyfra.core.GCodec.given
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.spirvtools.{SpirvToolsRunner, SpirvValidator}

class WorkgroupPrimitivesTest extends munit.FunSuite:
  case class Buffers(output: GBuffer[RuntimeArray[Int32]]) derives Layout

  private def run(body: Buffers => GIO ?=> Unit): Array[Int] =
    val program = GioProgram[Unit, Buffers](
      layout = _ => Buffers(GBuffer(256)),
      dispatch = (_, _) => GProgram.StaticDispatch((2, 1, 1)),
      workgroupSize = (128, 1, 1),
    )(body)
    val output = new Array[Int](256)
    given SpirvToolsRunner = SpirvToolsRunner(validator = SpirvValidator.Enable(throwOnFail = true))
    VkCyfraRuntime.using:
      summon[VkCyfraRuntime].withAllocation: allocation =>
        import allocation.*
        val result = GBuffer[RuntimeArray[Int32]](256)
        program.execute((), Buffers(result))
        result.readArray(output)
    output

  test("workgroup builtins and shared memory communicate across invocations"):
    val result = run: layout =>
      val local = GIO.localInvocationIndex
      val shared = SharedVariable[SizedArray[Int32, 128]]()
      GIO.write(shared.focus(_.at(local)), GIO.invocationId)
      GIO.barrier
      val neighbor = GIO.read(shared.focus(_.at((local + 1) % 128)))
      GIO.write(layout.output.focus(_.at(GIO.invocationId)), neighbor + GIO.workgroupId.x * 1000)
    assertEquals(result.toSeq, (0 until 256).map(i => (i / 128) * 1128 + (i + 1) % 128))

  test("subgroup reductions, scans and shuffles match lane arithmetic"):
    val result = run: layout =>
      val lane = GIO.subgroupLocalInvocationId
      val size = GIO.subgroupSize
      val sum = GIO.subgroupAdd(Int32(1))
      val prefix = GIO.subgroupInclusiveAdd(Int32(1))
      val exclusive = GIO.subgroupExclusiveAdd(Int32(1))
      val minimum = GIO.subgroupMin(lane)
      val maximum = GIO.subgroupMax(lane)
      val shuffled = GIO.subgroupShuffleXor(lane, Int32(1))
      val broadcast = GIO.subgroupBroadcast(lane, Int32(0))
      val correct =
        (sum === size) &&
          (prefix === lane + 1) &&
          (exclusive === lane) &&
          (minimum === 0) &&
          (maximum === size - 1) &&
          (shuffled === (lane ^ 1)) &&
          (broadcast === 0)
      GIO.write(layout.output.focus(_.at(GIO.invocationId)), GIO.branch(correct, Int32(1), Int32(0)))
    assertEquals(result.toSeq, Seq.fill(256)(1))

  test("nested unrolled folds preserve independent index and accumulator values"):
    val result = run: layout =>
      val base = GIO.eval(GIO.invocationId * 3 + 1)
      val value = GIO.foldRepeat[Int32](Int32(4), Int32(0), unroll = true): (i, sum) =>
        val inner = GIO.foldRepeat[Int32](Int32(3), Int32(0)): (j, acc) =>
          GIO.branch(i > j, acc + base, acc + 1)
        sum + inner
      GIO.write(layout.output.focus(_.at(GIO.invocationId)), value)
    assertEquals(result.toSeq, (0 until 256).map(i => 6 * (i * 3 + 1) + 6))
