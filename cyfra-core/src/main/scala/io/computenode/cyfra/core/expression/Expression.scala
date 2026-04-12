package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.memory.{FocusRoot, LocalVariable, Variable}
import io.computenode.cyfra.utility.Utility.nextId

import java.nio.{ByteBuffer, ByteOrder}
import scala.compiletime.constValue

sealed trait Expression[A: Value]:
  val id: Int = nextId()
  def v: Value[A] = Value[A]

object Expression:
  sealed trait ExpressionUnit[B: Value] extends Expression[Unit]:
    def v2: Value[B] = Value[B]

  case class Constant[A: Value](value: Any) extends Expression[A]
  case class LiteralArgs(value: List[Int]) extends Expression[Literal]
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
  case class Extract[In: Value, Res: Value](value: Expression[In], index: Int) extends Expression[Res]:
    def v2: Value[In] = Value[In]
  case class Combine[A: Value](composites: List[Expression[?]]) extends Expression[A]
  case class Insert[A: Value, Replace: Value](original: Expression[A], replacement: Expression[Replace], index: Int) extends Expression[A]:
    def v2: Value[Replace] = Value[Replace]

  def constantToByteBuffer[T: Value](value: T): Array[Byte] =
    val exp = Value[T].peel(value)
    exp.result match
      case x: Expression.Constant[a] => Expression.constantRec(x.value)
      case _                         => ???

  private def constantRec(const: Any): Array[Byte] = const match
    case x: UInt32  => constantToByteBuffer(x)
    case x: Int     => ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(x).array()
    case x: Product => x.productIterator.flatMap(constantRec).toArray
