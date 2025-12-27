package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}

import scala.collection.mutable
import io.computenode.cyfra.compiler.id

case class Compilation(context: Context, functions: List[FunctionIR[?]], functionBodies: List[IRs[?]]):
  def output: List[IR[?]] =
    context.output ++ functionBodies.flatMap(_.body)

object Compilation:
  def apply(functions: List[(FunctionIR[?], IRs[?])]): Compilation =
    val (f, fir) = functions.unzip
    Compilation(Context(Nil, new DebugManager, new TypeManager, new ConstantsManager), f, fir)

  def debugPrint(compilation: Compilation): Unit =
    val irs = compilation.output
    val map = irs.zipWithIndex.map(x => (x._1, s"%${x._2}")).toMap

    def irInternal(ir: IR[?]): String = ir match
      case IR.Constant(value)                               => s"($value)"
      case IR.VarDeclare(variable)                          => s"#${variable.id}"
      case IR.VarRead(variable)                             => s"#${variable.id}"
      case IR.VarWrite(variable, value)                     => s"#${variable.id} ${map(value)}"
      case IR.ReadBuffer(buffer, index)                     => s"@${buffer.id} ${map(index)}"
      case IR.WriteBuffer(buffer, index, value)             => s"@${buffer.id} ${map(index)} ${map(value)}"
      case IR.ReadUniform(uniform)                          => s"@${uniform.id}"
      case IR.WriteUniform(uniform, value)                  => s"@${uniform.id} ${map(value)}"
      case IR.Operation(func, args)                         => s"${func.name} ${args.map(map).mkString(" ")}"
      case IR.Call(func, args)                              => s"${func.name} ${args.map(_.id).mkString(" ")}"
      case IR.Branch(cond, ifTrue, ifFalse, break)          => s"${map(cond)} ???"
      case IR.Loop(mainBody, continueBody, break, continue) => "???"
      case IR.Jump(target, value)                           => s"${target.id} ${map(value)}"
      case IR.ConditionalJump(cond, target, value)          => s"${map(cond)} ${target.id} ${map(value)}"
      case IR.SvInst(op, operands)                          =>
        s"${op.mnemo} ${operands
            .map:
              case w: IR[?] => map(w)
              case w        => w.toString
            .mkString(" ")}"

    irs
      .map: ir =>
        val name = ir.getClass.getSimpleName
        val idStr = map(ir)
        s"${" ".repeat(5 - idStr.length) + idStr} = $name " + irInternal(ir)
      .foreach(println)
