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
      val shared = SharedVariable[GList[Float32, 128]]()
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

  test("nested folding loops preserve SSA scope"):
    val code = compile:
      val base = GIO.eval(GIO.invocationId * 3 + 1)
      val value = GIO.foldRepeat[Float32](Int32(4), Float32(0), unroll = true): (i, acc) =>
        val inner = GIO.foldRepeat[Float32](Int32(3), Float32(0)): (j, total) =>
          GIO.branch(i > j, total + base.asFloat, total + 1.0f)
        acc + inner
      GIO.write(layout.output.focus(_.at(GIO.invocationId)), value + base.asFloat)
    assert(instructions(code).filter(_._1 == 246).forall(_._2.last == 0))

  test("same-type scalar and vector conversions are identities"):
    val code = compile:
      val half = GIO.read(layout.half.focus(_.at(0))).asFloat16
      val single = GIO.read(layout.output.focus(_.at(0))).asFloat32
      GIO.write(layout.half.focus(_.at(1)), half)
      GIO.write(layout.half.focus(_.at(2)), Vec4(single).asFloat16.asFloat16.x)
      GIO.write(layout.output.focus(_.at(1)), Vec4(single).asFloat32.x)
    assertEquals(instructions(code).count { case (op, _) => op == 115 }, 1)

  test("direct GIO unrolls each iteration before compiler lowering"):
    val code = compile:
      GIO.repeat(Int32(3), unroll = true): i =>
        GIO.write(layout.output.focus(_.at(i)), i.asFloat)
        GIO.barrier
    val ops = instructions(code)
    assertEquals(ops.count(_._1 == 246), 0)
    assertEquals(ops.count(_._1 == 62), 3)
    assertEquals(ops.count(_._1 == 224), 3)

  test("unrolling a runtime iteration count is rejected"):
    intercept[IllegalArgumentException]:
      compile:
        GIO.repeat(GIO.invocationId, unroll = true): i =>
          GIO.write(layout.output.focus(_.at(i)), Float32(1))

  test("shared variables use cached fixed-list types and support scalar storage"):
    val code = compile:
      val a = SharedVariable[GList[Float32, 4]]()
      val b = SharedVariable[GList[Float32, 4]]()
      val c = SharedVariable[GList[Float32, 8]]()
      val scalar = SharedVariable[Float16]()
      val i = GIO.localInvocationIndex % 4
      GIO.write(a.focus(_.at(i)), Float32(2))
      GIO.write(b.focus(_.at(1)), Float32(3))
      GIO.write(c.focus(_.at(7)), Float32(4))
      GIO.write(scalar, Float16(1))
      GIO.barrier
      GIO.write(layout.output.focus(_.at(i)), GIO.read(a.focus(_.at(i))) + GIO.read(b.focus(_.at(1))) + GIO.read(scalar).asFloat32)
    val ops = instructions(code)
    val arrays = ops.filter(_._1 == 28)
    assertEquals(arrays.size, 2)
    val constants = ops.collect { case (43, List(_, id, n)) => id -> n }.toMap
    assertEquals(arrays.map(x => constants(x._2.last)).toSet, Set(4, 8))
    val arrayIds = arrays.map(_._2.head).toSet
    assertEquals(ops.count { case (op, args) => op == 32 && args(1) == 4 && arrayIds(args.last) }, 2)

  case class FixedFields(values: GList[Vec4[Float32], 4])

  test("fixed lists preserve nested focus paths and buffer layout"):
    case class FixedLayout(data: GBuffer[FixedFields]) derives Layout
    val ref = Layout[FixedLayout].layoutRef
    val body = GIO.reify:
      val shared = SharedVariable[FixedFields]()
      val i = GIO.localInvocationIndex % 4
      GIO.write(shared.focus(_.values.at(i).z), Float32(7))
      GIO.barrier
      GIO.write(ref.data.focus(_.values.at(i).z), GIO.read(shared.focus(_.values.at(i).z)))
    val code = new Compiler().compile(body, Compiler.Compute(Layout[FixedLayout].toBindings(ref), (128, 1, 1)))
    SpirvValidator.validateSpirv(code, SpirvValidator.Enable(throwOnFail = true))
    assert(instructions(code).exists { case (op, args) => op == 71 && args.tail == List(6, 16) })

  test("nested fixed lists work as local and shared variables"):
    val code = compile:
      val local = GIO.declare[GList[GList[Float32, 3], 2]]()
      val shared = SharedVariable[GList[GList[Float32, 3], 2]]()
      GIO.write(local.focus(_.at(1).at(2)), Float32(9))
      GIO.write(shared.focus(_.at(1).at(2)), GIO.read(local.focus(_.at(1).at(2))))
      GIO.barrier
      GIO.write(layout.output.focus(_.at(0)), GIO.read(shared.focus(_.at(1).at(2))))
    assertEquals(instructions(code).count(_._1 == 28), 2)

  test("zero unrolled iterations emit no effects and normal runtime repetition keeps a loop"):
    val code = compile:
      GIO.repeat(Int32(0), unroll = true): _ =>
        GIO.barrier
      GIO.repeat(GIO.invocationId): i =>
        GIO.write(layout.output.focus(_.at(i)), Float32(1))
    val ops = instructions(code)
    assertEquals(ops.count(_._1 == 224), 0)
    assertEquals(ops.count(_._1 == 246), 1)

  test("pointer-only half declarations register the required capability"):
    val body = GIO.reify:
      GIO.declare[Float16]()
      ()
    val code = new Compiler().compile(body, Compiler.Compute(Seq.empty, (128, 1, 1)))
    SpirvValidator.validateSpirv(code, SpirvValidator.Enable(throwOnFail = true))
    assert(instructions(code).exists { case (op, args) => op == 17 && args == List(9) })

  test("unused nested half bindings register storage capabilities without a read"):
    case class HalfFields(values: GList[Float16, 8])
    case class HalfLayout(uniform: GUniform[HalfFields], storage: GBuffer[HalfFields]) derives Layout
    val ref = Layout[HalfLayout].layoutRef
    val code = new Compiler().compile(GIO.reify(()), Compiler.Compute(Layout[HalfLayout].toBindings(ref), (128, 1, 1)))
    SpirvValidator.validateSpirv(code, SpirvValidator.Enable(throwOnFail = true))
    val capabilities = instructions(code).collect { case (17, List(capability)) => capability }.toSet
    assert(Set(9, 4433, 4434).subsetOf(capabilities))

  test("finalization orders shared array lengths and reuses shader constants"):
    case class FixedLayout(output: GBuffer[GArray[UInt32]]) derives Layout
    val ref = Layout[FixedLayout].layoutRef
    val body = GIO.reify:
      val a = SharedVariable[GList[Float32, 4]]()
      val b = SharedVariable[GList[UInt32, 4]]()
      GIO.write(a.focus(_.at(0)), Float32(1))
      GIO.write(b.focus(_.at(0)), UInt32(4))
      GIO.write(ref.output.focus(_.at(0)), GIO.read(b.focus(_.at(0))))
    val code = new Compiler().compile(body, Compiler.Compute(Layout[FixedLayout].toBindings(ref), (128, 1, 1)))
    SpirvValidator.validateSpirv(code, SpirvValidator.Enable(throwOnFail = true))
    val ops = instructions(code)
    val uintType = ops.collectFirst { case (21, List(id, 32, 0)) => id }.get
    val lengths = ops.collect { case (43, List(tpe, id, 4)) if tpe == uintType => id }
    assertEquals(lengths.size, 1)
    val arrays = ops.filter(_._1 == 28)
    assertEquals(arrays.size, 2)
    assert(arrays.forall(_._2.last == lengths.head))

  test("matrix constants register their vector and scalar dependencies"):
    compile:
      val local = GIO.declare[Mat2x2[Float32]]()
      GIO.write(local, Mat2x2[Float32](1.0f, 2.0f, 3.0f, 4.0f))
