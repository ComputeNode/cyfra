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

  case class TestLayout(in1: GBuffer[RuntimeArray[Int32]], in2: GUniform[Vec3[UInt32]]) derives Layout
  test("compile simple case"):
    val ref = Layout[TestLayout].layoutRef
    val config = Compiler.Compute(Layout[TestLayout].toBindings(ref), (1024, 1, 1))

    val TestLayout(b1, u1) = ref
    val exp = GIO.reify:
      val i = GIO.read(b1.focus(_.at(0)))
      GIO.write(b1.focus(_.at(1)), i + 10)
      GIO.write(b1.focus(_.at(i)), i * 10)

      val idx = GIO.read(GlobalInvocationId)
      GIO.write(u1, idx.yyy)
    compiler.compile(exp, config)
