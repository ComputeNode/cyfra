package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.memory.GUniform
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.core.expression.CustomFunction
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.ops.*
import io.computenode.cyfra.core.expression.ops.given
import io.computenode.cyfra.core.memory.*
import io.computenode.cyfra.core.memory.BuildInVariable.GlobalInvocationId
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.spirvtools.SpirvValidator
import io.computenode.cyfra.spirvtools.SpirvValidator.Enable

class CompilerTest extends munit.FunSuite:
  val compiler = new Compiler("all")

  case class TestLayout(in1: GBuffer[GArray[Int32]], in2: GUniform[Vec3[UInt32]]) derives Layout

  test("compile simple case"):
    val ref = Layout[TestLayout].layoutRef
    val config = Compiler.Compute(Layout[TestLayout].toBindings(ref), (1024, 1, 1))

    val TestLayout(b1, u1) = ref
    val exp = GIO.reify:
      val v = GIO.read(b1.focus(_.at(0)))
      val idx = GIO.read(GlobalInvocationId.focus(_.x))
      GIO.write(b1.focus(_.at(1)), v + 10)
      GIO.write(b1.focus(_.at(idx)), v * 10)

      val i = GIO.read(GlobalInvocationId)
      val x = i.x
//      val v2 = i.z(x)
//      GIO.write(u1, v2.yzx)
    val code = compiler.compile(exp, config)
    SpirvValidator.validateSpirv(code, Enable(throwOnFail = true))

  case class EmptyLayout()
  given Layout[EmptyLayout]:
    def fromBindings(bindings: Seq[GBinding[?]]): EmptyLayout = EmptyLayout()
    def toBindings(layout: EmptyLayout): Seq[GBinding[?]] = Seq.empty
    def layoutRef: EmptyLayout = EmptyLayout()

  private type Inner = (Float32, UInt32)
  private type Struct = (Int32, Inner, UInt32)

  test("compile struct"):
    val ref = Layout[EmptyLayout].layoutRef
    val config = Compiler.Compute(Layout[EmptyLayout].toBindings(ref), (1024, 1, 1))
    val init: Struct = (1, (2.2f, UInt32(3)), UInt32(4))

    val exp = GIO.reify:
      val variable: LocalVariable[Struct] = GIO.declare(Some(init))
      val variableInner = variable.focus(_._2)

      val x1 = GIO.read(variableInner).copy(_2 = UInt32(5))
      GIO.write(variableInner, x1)

      val x2 = GIO.read(variable).copy(_3 = UInt32(5))
      GIO.write(variable, x2)

      val x3: Struct = GIO.read(variable).copy(_1 = 10, _3 = UInt32(12))
      GIO.write(variable, x3)

    compiler.compile(exp, config)

  test("value shared by both branches and used after them"):
    val ref = Layout[TestLayout].layoutRef
    val config = Compiler.Compute(Layout[TestLayout].toBindings(ref), (1024, 1, 1))
    val TestLayout(b1, _) = ref

    val exp = GIO.reify:
      val v = GIO.read(b1.focus(_.at(0)))
      val shared = v * 3 + 1
      val picked = io.computenode.cyfra.dsl.Library.when(v > 0)(shared * 2).otherwise(shared - 1)
      GIO.write(b1.focus(_.at(1)), picked + shared)
    SpirvValidator.validateSpirv(compiler.compile(exp, config), Enable(throwOnFail = true))

  test("branch value reused in a nested elseWhen condition and after it"):
    import io.computenode.cyfra.dsl.Library.when
    val ref = Layout[TestLayout].layoutRef
    val exp = GIO.reify:
      val v = GIO.read(ref.in1.focus(_.at(0)))
      val d = when(v > 0)(v * 2).otherwise(v * 3)
      val r = when(v === 1)(Int32(5)).elseWhen(d === 4)(Int32(6)).otherwise(Int32(7))
      GIO.write(ref.in1.focus(_.at(1)), d + r)
    val code = compiler.compile(exp, Compiler.Compute(Layout[TestLayout].toBindings(ref), (1024, 1, 1)))
    SpirvValidator.validateSpirv(code, Enable(throwOnFail = true))

  test("variable initialised from a branch result"):
    import io.computenode.cyfra.dsl.Library.when
    val ref = Layout[TestLayout].layoutRef
    val exp = GIO.reify:
      val v = GIO.read(ref.in1.focus(_.at(0)))
      val x = GIO.declare(Some(when(v > 0)(v * 2).otherwise(v)))
      GIO.write(ref.in1.focus(_.at(1)), GIO.read(x))
    val code = compiler.compile(exp, Compiler.Compute(Layout[TestLayout].toBindings(ref), (1024, 1, 1)))
    SpirvValidator.validateSpirv(code, Enable(throwOnFail = true))

  test("custom function call references the right function"):
    val ref = Layout[TestLayout].layoutRef
    val f = CustomFunction[Int32, Int32](v => GIO.reify(GIO.read(v) + 1))
    val exp = GIO.reify:
      val x = GIO.read(ref.in1.focus(_.at(0)))
      val arg = GIO.declare[Int32]()
      GIO.write(arg, x)
      GIO.write(ref.in1.focus(_.at(1)), GIO.call(f, arg))
    val code = compiler.compile(exp, Compiler.Compute(Layout[TestLayout].toBindings(ref), (1024, 1, 1)))
    SpirvValidator.validateSpirv(code, Enable(throwOnFail = true))

//  test("compile vectors"):
//    val ref = Layout[EmptyLayout].layoutRef
//    val config = Compiler.Compute(Layout[EmptyLayout].toBindings(ref), (1024, 1, 1))
//    val init = Vec4[Float32](0.0f, 1.0f, 1.0f, 0.0f)
//
//    val exp = GIO.reify:
//      val vecVar: LocalVariable[Vec4[Float32]] = GIO.declare(init)
//      val vec3 = GIO.read(vecVar).yzx
//      val res = init.copy(zyx = vec3)
//      GIO.write(vecVar, res)
//    compiler.compile(exp, config)
