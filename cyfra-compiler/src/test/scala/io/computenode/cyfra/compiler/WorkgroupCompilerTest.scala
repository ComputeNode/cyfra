package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.core.expression.{ExpressionBlock, Value}
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.memory.*
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.spirvtools.SpirvValidator
import java.nio.{ByteBuffer, ByteOrder}

class WorkgroupCompilerTest extends munit.FunSuite:
  case class TestLayout(output: GBuffer[GArray[Float32]], half: GBuffer[GArray[Float16]]) derives Layout
  private val layout = Layout[TestLayout].layoutRef

  private def compile(body: GIO ?=> Unit): ByteBuffer =
    val code = new Compiler().compile(GIO.reify(body), Compiler.Compute(Layout[TestLayout].toBindings(layout), (128, 1, 1)))
    SpirvValidator.validateSpirv(code, SpirvValidator.Enable(throwOnFail = true))
    code

  private def instructions(code: ByteBuffer): List[(Int, List[Int])] =
    val ints = code.duplicate().order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
    ints.position(5)
    val out = List.newBuilder[(Int, List[Int])]
    while ints.hasRemaining do
      val header = ints.get()
      out += ((header & 0xffff, List.fill((header >>> 16) - 1)(ints.get())))
    out.result()

  test("shared arrays, barriers and subgroup collectives validate"):
    compile:
      val local = GIO.localInvocationIndex
      val shared = GShared[Float32](128)
      GIO.write(shared.focus(_.at(local)), local.asFloat)
      GIO.barrier
      val neighbor = GIO.read(shared.focus(_.at((local + 1) % 128)))
      val sum = GIO.subgroupAdd(neighbor)
      val minimum = GIO.subgroupMin(neighbor)
      val maximum = GIO.subgroupMax(neighbor)
      val prefix = GIO.subgroupInclusiveAdd(neighbor)
      val exclusive = GIO.subgroupExclusiveAdd(neighbor)
      val broadcast = GIO.subgroupBroadcast(sum, Int32(0))
      val first = GIO.subgroupBroadcastFirst(neighbor)
      val shuffled = GIO.subgroupShuffleXor(neighbor, Int32(1))
      val lane = GIO.subgroupLocalInvocationId
      val ids = GIO.workgroupId.x + GIO.localInvocationId.x + GIO.numWorkgroups.x + GIO.subgroupId + GIO.subgroupSize
      GIO.write(
        layout.output.focus(_.at(local)),
        sum + minimum + maximum + prefix + exclusive + broadcast + first + shuffled + lane.asFloat + ids.asFloat,
      )

  test("half constants contain half bits, and scalar and vector conversion validate"):
    val code = compile:
      GIO.write(layout.half.focus(_.at(0)), Float16(1.5f))
      val half = GIO.read(layout.half.focus(_.at(0)))
      val vector = Vec4(half.asFloat32).asFloat16.asFloat32
      GIO.write(layout.output.focus(_.at(0)), vector.x)
    val ops = instructions(code)
    val halfType = ops.collectFirst { case (22, List(id, 16)) => id }.get
    assert(ops.exists { case (op, args) => op == 43 && args.head == halfType && args.last == 0x3e00 })
    assert(ops.exists { case (op, args) => op == 17 && args == List(4433) })

  test("nested folding loops preserve SSA scope and unroll hint"):
    val code = compile:
      val base = GIO.eval(GIO.invocationId * 3 + 1)
      val value = GIO.foldRepeat[Float32](Int32(4), Float32(0), unroll = true): (i, acc) =>
        val inner = GIO.foldRepeat[Float32](Int32(3), Float32(0)): (j, total) =>
          GIO.branch(i > j, total + base.asFloat, total + 1.0f)
        acc + inner
      GIO.write(layout.output.focus(_.at(GIO.invocationId)), value + base.asFloat)
    assert(instructions(code).exists { case (op, args) => op == 246 && args.last == 1 })

  test("same-type scalar and vector conversions are identities"):
    val code = compile:
      val half = GIO.read(layout.half.focus(_.at(0))).asFloat16
      val single = GIO.read(layout.output.focus(_.at(0))).asFloat32
      GIO.write(layout.half.focus(_.at(1)), half)
      GIO.write(layout.half.focus(_.at(2)), Vec4(single).asFloat16.asFloat16.x)
      GIO.write(layout.output.focus(_.at(1)), Vec4(single).asFloat32.x)
    assertEquals(instructions(code).count { case (op, _) => op == 115 }, 1)
