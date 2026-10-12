package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.memory.{FocusRoot, LocalVariable, Variable}
import io.computenode.cyfra.utility.Utility.nextId

import izumi.reflect.Tag

import java.nio.{ByteBuffer, ByteOrder}
import scala.compiletime.constValue

sealed trait Expression[A: Value]:
  val id: Int = nextId()
  @volatile private[expression] var trusted: Boolean = false
  def v: Value[A] = Value[A]

object Expression:
  sealed trait ExpressionUnit[B: Value] extends Expression[Unit]:
    def v2: Value[B] = Value[B]

  sealed trait BranchingExpression

  case class Constant[A: Value](value: Any) extends Expression[A]
  case class LiteralArgs(value: List[Int]) extends Expression[Literal]
  case class VariableDeclare[B: Value](variable: LocalVariable[B], init: Option[Expression[B]]) extends ExpressionUnit[B]
  case class Read[A: Value](focus: FocusRoot[?], accessChain: List[Expression[?]]) extends Expression[A]
  case class Write[B: Value](focus: FocusRoot[?], accessChain: List[Expression[?]], value: Expression[B]) extends ExpressionUnit[B]
  case class Operation[A: Value](op: Operator, args: List[Expression[?]]) extends Expression[A]
  case class OperationImpure[A: Value](op: OperatorImpure, args: List[Expression[?]]) extends Expression[A]
  case class CustomCall[A: Value](func: CustomFunction[A], args: List[Variable[?]]) extends Expression[A] with BranchingExpression
  case class Branch[A: Value](cond: Expression[Bool], ifTrue: ExpressionBlock[A], ifFalse: ExpressionBlock[A], break: JumpTarget[A])
      extends Expression[A]
      with BranchingExpression
  case class Loop(mainBody: ExpressionBlock[Unit], continueBody: ExpressionBlock[Unit], break: BreakTarget, continue: ContinueTarget)
      extends Expression[Unit]
      with BranchingExpression // TODO change jump target to phi
  case class Jump[B: Value](target: JumpTarget[B], value: Expression[B]) extends ExpressionUnit[B]
  case class ConditionalJump[B: Value](cond: Expression[Bool], target: JumpTarget[B], value: Expression[B]) extends ExpressionUnit[B]
  case class Extract[In: Value, Res: Value](value: Expression[In], index: Int) extends Expression[Res]:
    def v2: Value[In] = Value[In]
  case class Combine[A: Value](composites: List[Expression[?]]) extends Expression[A]
  case class Insert[A: Value, Replace: Value](original: Expression[A], replacement: Expression[Replace], index: Int) extends Expression[A]:
    def v2: Value[Replace] = Value[Replace]

  val unitZero = Expression.Constant[Unit](())

  def constantToByteBuffer[T: Value](value: T): Array[Byte] =
    Value[T].peel(value).result match
      case x: Expression.Constant[a] => constantRec(x.value, Value[T])
      case _                         => throw IllegalArgumentException(s"Value is not a compile-time constant: $value")

  private def constantRec(const: Any, v: Value[?]): Array[Byte] =
    if v.baseTag.exists(_ <:< Tag[Tuple]) then
      const.asInstanceOf[Product].productIterator.zip(v.composites).flatMap((c, cv) => constantRec(c, cv)).toArray
    else if v.baseTag.isDefined then const.asInstanceOf[Product].productIterator.flatMap(constantRec(_, v.bottomComposite)).toArray
    else
      val bb = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
      v.tag match
        case t if t =:= Tag[Int32] || t =:= Tag[UInt32] => bb.putInt(const.asInstanceOf[Int])
        case t if t =:= Tag[Int16] || t =:= Tag[UInt16] => bb.putShort(const.asInstanceOf[Int].toShort)
        case t if t =:= Tag[Float32]                    => bb.putFloat(const.asInstanceOf[Float])
        case t if t =:= Tag[Float16]                    => bb.putShort(java.lang.Float.floatToFloat16(const.asInstanceOf[Float]))
        case t if t =:= Tag[Float64]                    => bb.putDouble(const.asInstanceOf[Double])
        case t                                          => throw IllegalArgumentException(s"$t has no buffer representation")
      bb.array().take(bb.position())
