package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.Expression
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.unitZero
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.utility.cats.Monad

import scala.util.boundary
import scala.util.boundary.break

case class ExpressionBlock[A](result: Expression[A], body: List[Expression[?]]):
  lazy val isPure: Boolean = isPureWith(Set.empty)

  def isPureWith(externalVarsIDs: Set[Int]): Boolean = boundary[Boolean]:
    body.foldRight(externalVarsIDs): (expr, vars) =>
      expr match
        case Expression.Constant(_)          => vars
        case Expression.VarDeclare(variable) => vars + variable.id
        case Expression.VarRead(variable)    =>
          if !vars.contains(variable.id) then break(false)
          vars
        case Expression.VarWrite(variable, _) =>
          if !vars.contains(variable.id) then break(false)
          vars
        case Expression.ReadBuffer(_, _)          => vars
        case Expression.WriteBuffer(_, _, _)      => break(false)
        case Expression.ReadUniform(_)            => vars
        case Expression.WriteUniform(_, _)        => break(false)
        case Expression.BuildInOperation(func, _) =>
          if !func.isPure then break(false)
          vars
        case Expression.CustomCall(func, _) =>
          if !func.isPure then break(false)
          vars
        case Expression.Branch(_, ifTrue, ifFalse, _) =>
          if !ifTrue.isPure then break(false)
          if !ifFalse.isPure then break(false)
          vars
        case Expression.Loop(mainBody, continueBody, _, _) =>
          if !mainBody.isPure then break(false)
          if !continueBody.isPure then break(false)
          vars
        case Expression.Jump(_, _)               => vars
        case Expression.ConditionalJump(_, _, _) => vars
    true

  def add[B](that: Expression[B]): ExpressionBlock[B] =
    ExpressionBlock(that, that :: this.body)

  def extend[B](that: ExpressionBlock[B]): ExpressionBlock[B] =
    ExpressionBlock(that.result, that.body ++ this.body)

  def traverse[T](f: Expression[?] => Option[T], enterFunctions: Boolean = false): List[Option[T]] =
    body.flatMap:
      case x @ Expression.Loop(mainBody, continueBody, _, _) =>
        continueBody.traverse(f, enterFunctions) ++ mainBody.traverse(f, enterFunctions) :+ f(x)
      case x @ Expression.Branch(_, ifTrue, ifFalse, _) =>
        ifFalse.traverse(f, enterFunctions) ++ ifTrue.traverse(f, enterFunctions) :+ f(x)
      case x @ Expression.CustomCall(func, _) if enterFunctions =>
        func.body.traverse(f, enterFunctions) :+ f(x)
      case other => List(f(other))

  def collect[T](pf: PartialFunction[Expression[?], T]): List[T] =
    traverse:
      case ir if pf.isDefinedAt(ir) => Some(pf(ir))
      case _                        => None
    .flatten

  def mkString: List[String] =
    traverse: x =>
      val prefix = s"%${x.id} = "
      val suffix = x match
        case Expression.Constant(value)                      => s"const $value"
        case Expression.VarDeclare(variable)                 => s"declare $variable"
        case Expression.VarRead(variable)                    => s"read $variable"
        case Expression.VarWrite(variable, value)            => s"write $variable <- %${value.id}"
        case Expression.ReadBuffer(buffer, index)            => s"read $buffer[%${index.id}]"
        case Expression.WriteBuffer(buffer, index, value)    => s"write $buffer[%${index.id}] <- %${value.id}"
        case Expression.ReadUniform(uniform)                 => s"read $uniform"
        case Expression.WriteUniform(uniform, value)         => s"write $uniform <- %${value.id}"
        case Expression.BuildInOperation(func, args)         => s"$func ${args.map(_.id).mkString("%", " %", "")}"
        case Expression.CustomCall(func, args)               => s"call #${func.id} ${args.map(_.id).mkString("%", " %", "")}"
        case Expression.Branch(cond, ifTrue, ifFalse, break) => s"branch %${cond.id} ? [%${ifTrue._1.id}] : [%${ifFalse._1.id}] -> jt#${break.id}"
        case Expression.Loop(mainBody, continueBody, break, continue) =>
          s"loop body[%${mainBody._1.id}] cont[%${continueBody._1.id}] break#${break.id} continue#${continue.id}"
        case Expression.Jump(target, value)                  => s"jump jt#${target.id} <- %${value.id}"
        case Expression.ConditionalJump(cond, target, value) => s"cjump %${cond.id} ? jt#${target.id} <- %${value.id}"
      Some(prefix + suffix)
    .flatten

object ExpressionBlock:
  def apply[A](expression: Expression[A]): ExpressionBlock[A] =
    ExpressionBlock(expression, List(expression))
  given Monad[ExpressionBlock] with
    def flatMap[A, B](fa: ExpressionBlock[A])(f: A => ExpressionBlock[B]): ExpressionBlock[B] =
      given t: Value[A] = fa.result.v
      val ExpressionBlock(res, body) = f(t.indirect(fa.result))
      ExpressionBlock(res, body ++ fa.body)
    def pure[A](x: A): ExpressionBlock[A] = x match
      case h: ExpressionHolder[A] => h.block
      case _: Unit                =>
        val zero = unitZero.asInstanceOf[Expression[A]]
        ExpressionBlock(zero, List(zero))
      case x: Any => ExpressionBlock[Any](Expression.Constant[Any](x), Nil).asInstanceOf[ExpressionBlock[A]]
