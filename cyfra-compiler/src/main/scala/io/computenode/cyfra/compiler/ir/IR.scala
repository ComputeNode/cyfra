package io.computenode.cyfra.compiler.ir

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.spirv.Opcodes.Code
import io.computenode.cyfra.compiler.spirv.Opcodes.Words
import io.computenode.cyfra.core.binding.{GBuffer, GUniform}
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given

import scala.collection

sealed trait IR[A: Value] extends Product:
  def v: Value[A] = summon[Value[A]]
  def substitute(map: collection.Map[RefIR[?], RefIR[?]]): IR[A] = replace(using map)
  def name: String = this.getClass.getSimpleName
  protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[A] = this

object IR:
  sealed trait RefIR[A: Value] extends IR[A]

  case class Constant[A: Value](value: Any) extends RefIR[A]
  case class VarDeclare[A: Value](variable: Var[A]) extends RefIR[Unit]
  case class VarRead[A: Value](variable: Var[A]) extends RefIR[A]
  case class VarWrite[A: Value](variable: Var[A], value: RefIR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[Unit] = this.copy(value = value.replaced)
  case class ReadBuffer[A: Value](buffer: GBuffer[A], index: RefIR[UInt32]) extends RefIR[A]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[A] = this.copy(index = index.replaced)
  case class WriteBuffer[A: Value](buffer: GBuffer[A], index: RefIR[UInt32], value: RefIR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[Unit] = this.copy(index = index.replaced, value = value.replaced)
  case class ReadUniform[A: Value](uniform: GUniform[A]) extends RefIR[A]
  case class WriteUniform[A: Value](uniform: GUniform[A], value: RefIR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[Unit] = this.copy(value = value.replaced)
  case class Operation[A: Value](func: BuildInFunction[A], args: List[RefIR[?]]) extends RefIR[A]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[A] = this.copy(args = args.map(_.replaced))
  case class Call[A: Value](func: FunctionIR[A], args: List[Var[?]]) extends RefIR[A]
  case class Branch[T: Value](cond: RefIR[Bool], ifTrue: IRs[T], ifFalse: IRs[T], break: JumpTarget[T]) extends IR[T]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[T] = this.copy(cond = cond.replaced)
  case class Loop(mainBody: IRs[Unit], continueBody: IRs[Unit], break: JumpTarget[Unit], continue: JumpTarget[Unit]) extends IR[Unit]
  case class Jump[A: Value](target: JumpTarget[A], value: RefIR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[Unit] = this.copy(value = value.replaced)
  case class ConditionalJump[A: Value](cond: RefIR[Bool], target: JumpTarget[A], value: RefIR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[RefIR[?], RefIR[?]]): IR[Unit] = this.copy(cond = cond.replaced, value = value.replaced)
  case class SvInst(op: Code, operands: List[Words | RefIR[?]]) extends IR[Unit]:
    override def name: String = op.mnemo
  case class SvRef[A: Value](op: Code, operands: List[Words | RefIR[?]]) extends RefIR[A]:
    override def name: String = op.mnemo

  extension [T](ir: RefIR[T])
    private def replaced(using map: collection.Map[RefIR[?], RefIR[?]]): RefIR[T] =
      map.getOrElse(ir, ir).asInstanceOf[RefIR[T]]
