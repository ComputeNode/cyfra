package io.computenode.cyfra.core.expression

import io.computenode.cyfra.utility.Utility.nextId

class CustomFunction[Res: Value] private[cyfra] (val name: String, val arg: List[Var[?]], val body: ExpressionBlock[Res]):
  def v: Value[Res] = summon[Value[Res]]
  val id: Int = nextId()
  lazy val isPure: Boolean = body.isPureWith(arg.map(_.id).toSet)

object CustomFunction:
  class CustomFunction1[Res: Value, A1: Value](name: String, arg: List[Var[?]], body: ExpressionBlock[Res])
      extends CustomFunction[Res](name, arg, body)

  def apply[A: Value, B: Value](func: Var[A] => ExpressionBlock[B]): CustomFunction1[B, A] =
    val arg = Var[A]()
    val declare = Expression.VarDeclare(arg)
    val ExpressionBlock(result, block) = func(arg)
    val body = ExpressionBlock(result, block.appended(declare))
    new CustomFunction1(s"custom${nextId() + 1}", List(arg), body)
