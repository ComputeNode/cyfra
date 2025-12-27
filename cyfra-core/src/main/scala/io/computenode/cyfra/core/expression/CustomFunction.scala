package io.computenode.cyfra.core.expression

import io.computenode.cyfra.utility.Utility.nextId

case class CustomFunction[A: Value] private[cyfra] (name: String, arg: List[Var[?]], body: ExpressionBlock[A]):
  def v : Value[A] = summon[Value[A]]
  val id: Int = nextId()
  lazy val isPure: Boolean = body.isPureWith(arg.map(_.id).toSet)

object CustomFunction:
  
  def apply[A: Value, B: Value](func: Var[A] => ExpressionBlock[B]): CustomFunction[B] =
    val arg = Var[A]()
    val body = func(arg)
    CustomFunction(s"custom${nextId() + 1}", List(arg), body)
