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
  def substitute(map: collection.Map[IR[?], IR[?]]): Unit = replace(using map)
  protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit = ()

object IR:
  case class Constant[A: Value](value: Any) extends IR[A]
  case class VarDeclare[A: Value](variable: Var[A]) extends IR[Unit]
  case class VarRead[A: Value](variable: Var[A]) extends IR[A]
  case class VarWrite[A: Value](variable: Var[A], var value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      value = value.replaced
  case class ReadBuffer[A: Value](buffer: GBuffer[A], var index: IR[UInt32]) extends IR[A]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      index = index.replaced
  case class WriteBuffer[A: Value](buffer: GBuffer[A], var index: IR[UInt32], var value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      index = index.replaced
      value = value.replaced
  case class ReadUniform[A: Value](uniform: GUniform[A]) extends IR[A]
  case class WriteUniform[A: Value](uniform: GUniform[A], var value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      value = value.replaced
  case class Operation[A: Value](func: BuildInFunction[A], var args: List[IR[?]]) extends IR[A]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      args = args.map(_.replaced)
  case class Call[A: Value](func: Function[A], args: List[Var[?]]) extends IR[A]
  case class Branch[T: Value](var cond: IR[Bool], ifTrue: IRs[T], ifFalse: IRs[T], var break: JumpTarget[T]) extends IR[T]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      cond = cond.replaced
  case class Loop(mainBody: IRs[Unit], continueBody: IRs[Unit], break: JumpTarget[Unit], continue: JumpTarget[Unit]) extends IR[Unit]
  case class Jump[A: Value](target: JumpTarget[A], var value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      value = value.replaced
  case class ConditionalJump[A: Value](var cond: IR[Bool], target: JumpTarget[A], var value: IR[A]) extends IR[Unit]:
    override protected def replace(using map: collection.Map[IR[?], IR[?]]): Unit =
      cond = cond.replaced
      value = value.replaced
  case class Instruction[A: Value](op: Code, operands: List[Words | IR[?]]) extends IR[A]

  extension [T](ir: IR[T])
    private def replaced(using map: collection.Map[IR[?], IR[?]]): IR[T] =
      map.getOrElse(ir, ir).asInstanceOf[IR[T]]
