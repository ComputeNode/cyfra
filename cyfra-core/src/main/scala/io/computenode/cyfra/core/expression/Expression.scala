package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.binding.{GBuffer, GUniform}
import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.utility.Utility.nextId
import io.computenode.cyfra.core.expression.{Bool, Float16, Float32, Int16, Int32, UInt16, UInt32, given}

import scala.Tuple.Elem
import scala.compiletime.constValue

sealed trait Expression[A: Value]:
  val id: Int = nextId()
  def v: Value[A] = Value[A]

object Expression:
  case class Constant[A: Value](value: Any) extends Expression[A]
  case class VarDeclare[A: Value](variable: Var[A]) extends Expression[Unit]:
    def v2: Value[A] = Value[A]
  case class VarRead[A: Value](variable: Var[A]) extends Expression[A]
  case class VarWrite[A: Value](variable: Var[A], value: Expression[A]) extends Expression[Unit]:
    def v2: Value[A] = Value[A]
  case class ReadBuffer[A: Value](buffer: GBuffer[A], index: Expression[UInt32]) extends Expression[A]
  case class WriteBuffer[A: Value](buffer: GBuffer[A], index: Expression[UInt32], value: Expression[A]) extends Expression[Unit]:
    def v2: Value[A] = Value[A]
  case class ReadUniform[A: Value](uniform: GUniform[A]) extends Expression[A]
  case class WriteUniform[A: Value](uniform: GUniform[A], value: Expression[A]) extends Expression[Unit]:
    def v2: Value[A] = Value[A]
  case class BuildInOperation[A: Value](func: BuildInFunction[A], args: List[Expression[?]]) extends Expression[A]
  case class CustomCall[A: Value](func: CustomFunction[A], args: List[Var[?]]) extends Expression[A]
  case class Branch[T: Value](cond: Expression[Bool], ifTrue: ExpressionBlock[T], ifFalse: ExpressionBlock[T], break: JumpTarget[T])
      extends Expression[T]
  case class Loop(mainBody: ExpressionBlock[Unit], continueBody: ExpressionBlock[Unit], break: BreakTarget, continue: ContinueTarget)
      extends Expression[Unit]
  case class Jump[A: Value](target: JumpTarget[A], value: Expression[A]) extends Expression[Unit]:
    def v2: Value[A] = Value[A]
  case class ConditionalJump[A: Value](cond: Expression[Bool], target: JumpTarget[A], value: Expression[A]) extends Expression[Unit]:
    def v2: Value[A] = Value[A]
  case class Composite[A <: Tuple: Value, N <: Int](value: Expression[A], n: N)
      extends Expression[Elem[A, N]](using value.v.composite(n).asInstanceOf[Value[Elem[A, N]]]):
    def v2: Value[A] = Value[A]
