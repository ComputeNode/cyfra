package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.Expression
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.unitZero
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.memory.{FocusConstant, FocusDynamic, FocusRoot, GBinding, GUniform, LocalVariable, Variable}
import io.computenode.cyfra.utility.cats.Monad

import scala.util.boundary
import scala.collection.mutable
import scala.util.boundary.break

case class ExpressionBlock[A](result: Expression[A], body: List[Expression[?]]):
  lazy val isPure: Boolean = isPureWith(Set.empty)

  def isPureWith(externalVarsIDs: Set[Int]): Boolean = boundary[Boolean]:
    body.foldRight(externalVarsIDs): (expr, vars) =>
      expr match
        case Expression.Constant(_)                  => vars
        case Expression.LiteralArgs(_)               => vars
        case Expression.VariableDeclare(variable, _) =>
          vars + variable.id
        case Expression.Read(focus, _) =>
          focus.getRoot match
            case variable: LocalVariable[?] if vars.contains(variable.id) => vars
            case uniform: GUniform[?]                                     => vars
            case _                                                        => break(false)
        case Expression.Write(focus, _, _) =>
          focus.getRoot match
            case variable: LocalVariable[?] if vars.contains(variable.id) => vars
            case _                                                        => break(false)
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
        case Expression.Extract(_, _)            => vars
        case Expression.Insert(_, _, _)          => vars
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
        case Expression.VariableDeclare(variable, init)      => s"declare $variable ${init.map(_.id.toString).getOrElse("")}"
        case Expression.Read(variable, accessChain)          => s"read $variable ${accessChain.map(_.id).mkString("%", " %", "")}"
        case Expression.Write(variable, accessChain, value)  => s"write $variable ${accessChain.map(_.id).mkString("%", " %", "")} <- %${value.id}"
        case Expression.BuildInOperation(func, args)         => s"$func ${args.map(_.id).mkString("%", " %", "")}"
        case Expression.CustomCall(func, args)               => s"call #${func.id} ${args.map(_.id).mkString("%", " %", "")}"
        case Expression.Branch(cond, ifTrue, ifFalse, break) => s"branch %${cond.id} ? [%${ifTrue._1.id}] : [%${ifFalse._1.id}] -> jt#${break.id}"
        case Expression.Loop(mainBody, continueBody, break, continue) =>
          s"loop body[%${mainBody._1.id}] cont[%${continueBody._1.id}] break#${break.id} continue#${continue.id}"
        case Expression.Jump(target, value)                  => s"jump jt#${target.id} <- %${value.id}"
        case Expression.ConditionalJump(cond, target, value) => s"cjump %${cond.id} ? jt#${target.id} <- %${value.id}"
        case Expression.Extract(value, n)                    => s"comp ${value.id} $n"
      Some(prefix + suffix)
    .flatten

object ExpressionBlock:
  def apply[A](expression: Expression[A]): ExpressionBlock[A] =
    ExpressionBlock(expression, List(expression))

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

  given Monad[ExpressionBlock] with
    def flatMap[A, B](fa: ExpressionBlock[A])(f: A => ExpressionBlock[B]): ExpressionBlock[B] = ExpressionBlock.flatMap(fa)(f)
    def pure[A](x: A): ExpressionBlock[A] = ExpressionBlock.pure(x)

  def optimise[A: Value](block: ExpressionBlock[A]): ExpressionBlock[A] =
    val combined = simplifyExtractCombine(block)
    val distinct = ExpressionBlock(combined.result, combined.body.reverse.distinctBy(_.id).reverse)
    val active = getActive(distinct)
    filterNotActive(distinct, active)

  private def simplifyExtractCombine[A: Value](block: ExpressionBlock[A]): ExpressionBlock[A] =
    block

  private def filterNotActive[A: Value](block: ExpressionBlock[A], active: Set[Int]): ExpressionBlock[A] = block // TODO filter

  private def getActive(block: ExpressionBlock[?]): Set[Int] =
    val visited = mutable.Set.empty[Int]

    def visit(current: Expression[?]): Unit =
      if visited(current.id) then return

      visited.add(current.id)
      current match
        case Expression.VariableDeclare(_, Some(x))  => visit(x)
        case Expression.Write(_, accessChain, value) =>
          accessChain.foreach(visit)
          visit(value)
        case Expression.Jump(_, value)                  => visit(value)
        case Expression.ConditionalJump(cond, _, value) =>
          visit(value)
          visit(cond)
        case Expression.Read(_, accessChain)             => accessChain.foreach(visit)
        case Expression.BuildInOperation(_, args)        => args.foreach(visit)
        case Expression.Branch(cond, ifTrue, ifFalse, _) =>
          visit(cond)
          visit(ifTrue.result)
          visit(ifFalse.result)
          visitBlock(ifTrue)
          visitBlock(ifFalse)
        case Expression.Loop(mainBody, continueBody, _, _) =>
          visitBlock(mainBody)
          visitBlock(continueBody)
        case Expression.Extract(value, _)                => visit(value)
        case Expression.Combine(composites)              => composites.foreach(visit)
        case Expression.Insert(original, replacement, _) =>
          visit(original)
          visit(replacement)
        case _ => ()

    def visitBlock(block: ExpressionBlock[?]): Unit =
      block.body.foreach:
        case x: Expression.VariableDeclare[?]                    => visit(x)
        case x: Expression.Write[?]                              => visit(x)
        case x: Expression.BuildInOperation[?] if !x.func.isPure => visit(x)
        case x: Expression.CustomCall[?] if !x.func.isPure       => visit(x)
        case x: Expression.Branch[?]                             => visit(x)
        case x: Expression.Loop                                  => visit(x)
        case x: Expression.Jump[?]                               => visit(x)
        case x: Expression.ConditionalJump[?]                    => visit(x)
        case _                                                   => ()

    visitBlock(block)
    visited.toSet
