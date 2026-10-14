package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.Expression
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.Expression.unitZero
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.memory.{FocusConstant, FocusDynamic, FocusRoot, GBinding, GUniform, LocalVariable, Variable}
import io.computenode.cyfra.utility.cats.Monad

import scala.util.boundary
import scala.collection.mutable
import scala.util.boundary.break

case class ExpressionBlock[A](result: Expression[A], body: List[Expression[?]]):
  lazy val isPure: Boolean = isPureWith(Set.empty)

  def isPureWith(externalVarsIDs: Set[Int]): Boolean = freeVariables.exists(_.subsetOf(externalVarsIDs))

  lazy val freeVariables: Option[Set[Int]] = boundary:
    val (_, free) = body.foldRight((Set.empty[Int], Set.empty[Int])): (expr, acc) =>
      val (declared, free) = acc
      def use(id: Int) = (declared, if declared(id) then free else free + id)
      def nested(blocks: ExpressionBlock[?]*) =
        (declared, blocks.foldLeft(free)((f, b) => f ++ (b.freeVariables.getOrElse(break(None)) -- declared)))
      expr match
        case e if e.trusted                          => acc
        case Expression.VariableDeclare(variable, _) => (declared + variable.id, free)
        case Expression.Read(focus, _)               =>
          focus.getRoot match
            case variable: LocalVariable[?] => use(variable.id)
            case _: GUniform[?]             => acc
            case _                          => break(None)
        case Expression.Write(focus, _, _) =>
          focus.getRoot match
            case variable: LocalVariable[?] => use(variable.id)
            case _                          => break(None)
        case Expression.CustomCall(func, _) =>
          if !func.isPure then break(None)
          acc
        case Expression.Branch(_, ifTrue, ifFalse, _)         => nested(ifTrue, ifFalse)
        case Expression.Loop(mainBody, continueBody, _, _, _) => nested(mainBody, continueBody)
        case Expression.OperationImpure(_, _)                 => break(None)
        case Expression.Operation(
              Operator.WorkgroupBarrier | _: Operator.SubgroupAdd | Operator.SubgroupMin | Operator.SubgroupMax | Operator.SubgroupBroadcast |
              Operator.SubgroupBroadcastFirst | Operator.SubgroupShuffle | Operator.SubgroupShuffleXor,
              _,
            ) =>
          break(None)
        case _: (Expression.Constant[?] | Expression.LiteralArgs | Expression.Operation[?] | Expression.Jump[?] | Expression.ConditionalJump[?] |
              Expression.Extract[?, ?] | Expression.Insert[?, ?] | Expression.Combine[?]) =>
          acc
    Some(free)

  def add[B](that: Expression[B]): ExpressionBlock[B] =
    ExpressionBlock(that, that :: this.body)

  def extend[B](that: ExpressionBlock[B]): ExpressionBlock[B] =
    ExpressionBlock(that.result, that.body ++ this.body)

  def traverse[T](f: Expression[?] => Option[T], enterFunctions: Boolean = false): List[Option[T]] =
    val visited = mutable.HashSet.empty[Int]
    def rec(block: ExpressionBlock[?]): List[Option[T]] =
      block.body.flatMap:
        case x if !visited.add(x.id)           => Nil
        case x: Expression.BranchingExpression =>
          x match
            case x @ Expression.Loop(mainBody, continueBody, _, _, _) => rec(continueBody) ++ rec(mainBody) :+ f(x)
            case x @ Expression.Branch(_, ifTrue, ifFalse, _)         => rec(ifFalse) ++ rec(ifTrue) :+ f(x)
            case x @ Expression.CustomCall(func, _) if enterFunctions => rec(func.body) :+ f(x)
            case x: Expression.CustomCall[?]                          => List(f(x))
        case other => List(f(other))
    rec(this)

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
        case Expression.Operation(func, args)                => s"$func ${args.map(_.id).mkString("%", " %", "")}"
        case Expression.CustomCall(func, args)               => s"call #${func.id} ${args.map(_.id).mkString("%", " %", "")}"
        case Expression.Branch(cond, ifTrue, ifFalse, break) => s"branch %${cond.id} ? [%${ifTrue._1.id}] : [%${ifFalse._1.id}] -> jt#${break.id}"
        case Expression.Loop(mainBody, continueBody, break, continue, unroll) =>
          s"loop body[%${mainBody._1.id}] cont[%${continueBody._1.id}] break#${break.id} continue#${continue.id}"
        case Expression.Jump(target, value)                  => s"jump jt#${target.id} <- %${value.id}"
        case Expression.ConditionalJump(cond, target, value) => s"cjump %${cond.id} ? jt#${target.id} <- %${value.id}"
        case Expression.Extract(value, n)                    => s"comp ${value.id} $n"
      Some(prefix + suffix)
    .flatten

object ExpressionBlock:
  def trust(block: ExpressionBlock[?]): Unit = block.body.foreach(_.trusted = true)

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
