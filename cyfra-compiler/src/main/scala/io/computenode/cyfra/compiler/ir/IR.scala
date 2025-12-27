package io.computenode.cyfra.compiler.ir

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.spirv.Opcodes.Code
import io.computenode.cyfra.compiler.spirv.Opcodes.Words
import io.computenode.cyfra.core.binding.{GBuffer, GUniform}
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given

import scala.collection

sealed trait IR[A: Value] extends Product:
  def v: Value[A] = summon[Value[A]]
  def substitute(map: collection.Map[IR[?], IR[?]]): IR[A] = replace(using map)
  def name: String = this.getClass.getSimpleName
  protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[A] = this

object IR:
  trait Ref

  case class Constant[A: Value](value: Any) extends IR[A] with Ref
  case class VarDeclare[A: Value](variable: Var[A]) extends IR[Unit] with Ref
  case class VarRead[A: Value](variable: Var[A]) extends IR[A] with Ref
  case class VarWrite[A: Value](variable: Var[A], value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[Unit] = this.copy(value = value.replaced)
  case class ReadBuffer[A: Value](buffer: GBuffer[A], index: IR[UInt32]) extends IR[A] with Ref:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[A] = this.copy(index = index.replaced)
  case class WriteBuffer[A: Value](buffer: GBuffer[A], index: IR[UInt32], value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[Unit] = this.copy(index = index.replaced, value = value.replaced)
  case class ReadUniform[A: Value](uniform: GUniform[A]) extends IR[A] with Ref
  case class WriteUniform[A: Value](uniform: GUniform[A], value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[Unit] = this.copy(value = value.replaced)
  case class Operation[A: Value](func: BuildInFunction[A], args: List[IR[?]]) extends IR[A] with Ref:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[A] = this.copy(args = args.map(_.replaced))
  case class Call[A: Value](func: FunctionIR[A], args: List[Var[?]]) extends IR[A] with Ref
  case class Branch[T: Value](cond: IR[Bool], ifTrue: IRs[T], ifFalse: IRs[T], break: JumpTarget[T]) extends IR[T]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[T] = this.copy(cond = cond.replaced)
  case class Loop(mainBody: IRs[Unit], continueBody: IRs[Unit], break: JumpTarget[Unit], continue: JumpTarget[Unit]) extends IR[Unit]
  case class Jump[A: Value](target: JumpTarget[A], value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[Unit] = this.copy(value = value.replaced)
  case class ConditionalJump[A: Value](cond: IR[Bool], target: JumpTarget[A], value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): IR[Unit] = this.copy(cond = cond.replaced, value = value.replaced)
  case class SvInst(op: Code, operands: List[Words | IR[?]]) extends IR[Unit]:
    override def name: String = op.mnemo
  case class SvRef[A: Value](op: Code, operands: List[Words | IR[?]]) extends IR[A] with Ref:
    override def name: String = op.mnemo

  extension [T](ir: IR[T])
    private def replaced(using map: collection.Map[IR[?], IR[?]]): IR[T] =
      map.getOrElse(ir, ir).asInstanceOf[IR[T]]
