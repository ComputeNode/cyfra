package foton

import io.computenode.cyfra.core.binding.{BufferRef, GBuffer}
import io.computenode.cyfra.dsl.direct.GIO.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.ops.*
import io.computenode.cyfra.core.expression.ops.given
import io.computenode.cyfra.core.expression.CustomFunction
import io.computenode.cyfra.core.expression.JumpTarget.BreakTarget
import io.computenode.cyfra.core.expression.JumpTarget.ContinueTarget
import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import izumi.reflect.Tag

case class SimpleLayout(in: GBuffer[Int32]) extends Layout

val funcFlow = CustomFunction[Int32, Unit]: iv =>
  reify:

    val body: (BreakTarget, ContinueTarget, GIO) ?=> Unit =
      val i = read(iv)
      conditionalBreak(i >= const[Int32](10))
      conditionalContinue(i >= const[Int32](5))
      val j = i + const[Int32](1)
      write(iv, j)

    val continue: GIO ?=> Unit =
      val i = read(iv)
      val j = i + const[Int32](1)
      write(iv, j)

    loop(body, continue)

    val ci = read(iv) > const[Int32](5)

    val ifTrue: (JumpTarget[Int32], GIO) ?=> Int32 =
      conditionalJump(const(true), const[Int32](32))
      const[Int32](16)

    val ifFalse: (JumpTarget[Int32], GIO) ?=> Int32 =
      jump(const[Int32](4))
      const[Int32](8)

    branch[Int32](ci, ifTrue, ifFalse)

    const[Unit](())

def readFunc(buffer: GBuffer[Int32]) = CustomFunction[UInt32, Int32]: in =>
  reify:
    val i = read(in)
    val a = read(buffer, i)
    val b = read(buffer, i + const(1))
    val c = a + b
    write(buffer, i + const(2), c)
    c

def program(buffer: GBuffer[Int32])(using GIO): Unit =
  val vA = declare[UInt32]()
  write(vA, const(0))
  call(readFunc(buffer), vA)
  call(funcFlow, vA)
  ()

@main
def main(): Unit =
  println("Foton Animation Module Loaded")
  val compiler = io.computenode.cyfra.compiler.Compiler(verbose = true)
  val p1 = (l: SimpleLayout) =>
    reify:
      program(l.in)
  val ls = LayoutStruct[SimpleLayout](SimpleLayout(BufferRef(0, summon[Tag[Int32]])), Nil)
  val rf = ls.layoutRef
  val lb = summon[LayoutBinding[SimpleLayout]].toBindings(rf)
  val body = p1(rf)
  compiler.compile(lb, body)

def const[A: Value](a: Any): A =
  summon[Value[A]].extract(ExpressionBlock(Expression.Constant(a)))
