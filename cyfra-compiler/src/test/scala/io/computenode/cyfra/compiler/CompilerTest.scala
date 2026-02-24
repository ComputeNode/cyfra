package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.memory.GUniform
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.ops.*
import io.computenode.cyfra.core.expression.ops.given
import io.computenode.cyfra.core.memory.*
import io.computenode.cyfra.core.memory.BuildInVariable.GlobalInvocationId
import io.computenode.cyfra.dsl.direct.GIO

class CompilerTest extends munit.FunSuite:
  val compiler = new Compiler("all")

  case class TestLayout(in1: GBuffer[Tuple1[RuntimeArray[Int32]]], in2: GUniform[Tuple1[Vec3[UInt32]]]) derives Layout

  test("compile simple case"):
    val ref = Layout[TestLayout].layoutRef
    val config = Compiler.Compute(Layout[TestLayout].toBindings(ref), (1024, 1, 1))

    val TestLayout(x1, x2) = ref
    val b1 = x1.focus(_._1)
    val u1 = x2.focus(_._1)
    val exp = GIO.reify:
      val v = GIO.read(b1.focus(_.at(0)))
      val idx = GIO.read(GlobalInvocationId.focus(_.x))
      GIO.write(b1.focus(_.at(1)), v + 10)
      GIO.write(b1.focus(_.at(idx)), v * 10)

      val i = GIO.read(GlobalInvocationId)
      val x = i.x
      val v2 = i.z(x)
      GIO.write(u1, v2.yzx)
    compiler.compile(exp, config)

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

//      val x1 = GIO.read(variableInner).copy(_2 = UInt32(5))
//      GIO.write(variableInner, x1)
//
//      val x2 = GIO.read(variable).copy(_3 = UInt32(5))
//      GIO.write(variable, x2)
//
//      val x3: Struct = GIO.read(variable).copy(_1 = 10, _3 = UInt32(12))
//      GIO.write(variable, x3)

    compiler.compile(exp, config)
