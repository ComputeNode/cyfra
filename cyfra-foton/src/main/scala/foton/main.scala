package foton

import io.computenode.cyfra.core.binding.{BufferRef, GBuffer}
import io.computenode.cyfra.dsl.direct.GIO.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.ops.*
import io.computenode.cyfra.core.expression.ops.given
import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import izumi.reflect.Tag

case class SimpleLayout(in: GBuffer[Int32]) extends Layout

def program(buffer: GBuffer[Int32])(using GIO): Unit =
  val a = read(buffer, UInt32(0))
  val b = read(buffer, UInt32(1))
  val c = a + b
  write(buffer, UInt32(2), c)

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
