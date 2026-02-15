package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.memory.{Focus, FocusRoot, GBuffer, GUniform, LocalVariable, Variable}
import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.utility.Utility.nextId
import io.computenode.cyfra.core.expression.given

import scala.Tuple.Elem
import scala.compiletime.constValue

sealed trait Expression[A: Value]:
  val id: Int = nextId()
  def v: Value[A] = Value[A]

object Expression:
  sealed trait ExpressionUnit[B: Value] extends Expression[Unit]:
    def v2: Value[B] = Value[B]

  case class Constant[A: Value](value: Any) extends Expression[A]
  case class VariableDeclare[B: Value](variable: LocalVariable[B], init: Option[Expression[B]]) extends ExpressionUnit[B]
  case class Read[A: Value](focus: FocusRoot[?], accessChain: List[Expression[?]]) extends Expression[A]
  case class Write[B: Value](focus: FocusRoot[?], accessChain: List[Expression[?]], value: Expression[B]) extends ExpressionUnit[B]
  case class BuildInOperation[A: Value](func: BuildInFunction, args: List[Expression[?]]) extends Expression[A]
  case class CustomCall[A: Value](func: CustomFunction[A], args: List[Variable[?]]) extends Expression[A]
  case class Branch[A: Value](cond: Expression[Bool], ifTrue: ExpressionBlock[A], ifFalse: ExpressionBlock[A], break: JumpTarget[A])
      extends Expression[A]
  case class Loop(mainBody: ExpressionBlock[Unit], continueBody: ExpressionBlock[Unit], break: BreakTarget, continue: ContinueTarget)
      extends Expression[Unit]
  case class Jump[B: Value](target: JumpTarget[B], value: Expression[B]) extends ExpressionUnit[B]
  case class ConditionalJump[B: Value](cond: Expression[Bool], target: JumpTarget[B], value: Expression[B]) extends ExpressionUnit[B]
  case class Composite[B <: Tuple: Value, N <: Int](value: Expression[B], n: N)
      extends Expression[Elem[B, N]](using value.v.composite(n).asInstanceOf[Value[Elem[B, N]]]):
    def v2: Value[B] = Value[B]
