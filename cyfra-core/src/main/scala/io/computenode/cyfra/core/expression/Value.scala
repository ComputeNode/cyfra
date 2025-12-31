package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock}
import io.computenode.cyfra.core.expression.BuildInFunction.{BuildInFunction0, BuildInFunction1, BuildInFunction2, BuildInFunction3, BuildInFunction4}
import io.computenode.cyfra.utility.cats.Monad
import izumi.reflect.Tag

trait Value[A]:
  def indirect(ir: Expression[A]): A = extract(ExpressionBlock(ir, List()))
  def extract(block: ExpressionBlock[A]): A =
    if !block.isPure then throw RuntimeException("Cannot embed impure expression")
    extractUnsafe(block)
  def composite: Option[Value[?]] = None
    
  protected def extractUnsafe(ir: ExpressionBlock[A]): A
  def tag: Tag[A]

  def peel(x: A): ExpressionBlock[A] =
    summon[Monad[ExpressionBlock]].pure(x)

object Value:
  def apply[A](using v: Value[A]): Value[A] = v
  
  def map[Res: Value as vr](f: BuildInFunction0[Res]): Res =
    val next = Expression.BuildInOperation(f, Nil)
    vr.extract(ExpressionBlock(next, List(next)))

  extension [A: Value as v](x: A)
    def map[Res: Value as vb](f: BuildInFunction1[A, Res]): Res =
      val arg = v.peel(x)
      val next = Expression.BuildInOperation(f, List(arg.result))
      vb.extract(arg.add(next))

    def map[A2: Value as v2, Res: Value as vb](x2: A2)(f: BuildInFunction2[A, A2, Res]): Res =
      val arg1 = v.peel(x)
      val arg2 = summon[Value[A2]].peel(x2)
      val next = Expression.BuildInOperation(f, List(arg1.result, arg2.result))
      vb.extract(arg1.extend(arg2).add(next))

    def map[A2: Value as v2, A3: Value as v3, Res: Value as vb](x2: A2, x3: A3)(f: BuildInFunction3[A, A2, A3, Res]): Res =
      val arg1 = v.peel(x)
      val arg2 = summon[Value[A2]].peel(x2)
      val arg3 = summon[Value[A3]].peel(x3)
      val next = Expression.BuildInOperation(f, List(arg1.result, arg2.result, arg3.result))
      vb.extract(arg1.extend(arg2).extend(arg3).add(next))

    def map[A2: Value as v2, A3: Value as v3, A4: Value as v4, Res: Value as vb](x2: A2, x3: A3, x4: A4)(
      f: BuildInFunction4[A, A2, A3, A4, Res],
    ): Res =
      val arg1 = v.peel(x)
      val arg2 = summon[Value[A2]].peel(x2)
      val arg3 = summon[Value[A3]].peel(x3)
      val arg4 = summon[Value[A4]].peel(x4)
      val next = Expression.BuildInOperation(f, List(arg1.result, arg2.result, arg3.result, arg4.result))
      vb.extract(arg1.extend(arg2).extend(arg3).extend(arg4).add(next))

    def irs: ExpressionBlock[A] = v.peel(x)
