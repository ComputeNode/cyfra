package io.computenode.cyfra.core

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import izumi.reflect.{Tag, TagK}

val v1: Var[S1] = new Var()

@main
def run(): Unit =
//  v1._1 // ???

  println("runncing")

type S1 = (Int32, Float32)

given Value[S1] = new Value:
  protected def extractUnsafe(ir: ExpressionBlock[S1]): S1 =
    val a = Expression.Composite[S1, 0](ir.result, 0)
    val b = Expression.Composite[S1, 1](ir.result, 1)
    (a.v.extract(ir.add(a)), b.v.extract(ir.add(b)))

  def tag: Tag[S1] = Tag[S1]
  def baseTag: Option[TagK[?]] = Some(Tag[Tuple])
  def composite: List[Value[?]] = List(Value[Int32], Value[Float32])
