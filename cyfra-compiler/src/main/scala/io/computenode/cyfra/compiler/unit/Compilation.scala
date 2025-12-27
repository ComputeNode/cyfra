package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.compiler.unit.Context

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
    val map = irs.filter(_.isInstanceOf[IR.Ref]).zipWithIndex.map(x => (x._1, s"%${x._2}")).toMap

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
      case sv: (IR.SvInst | IR.SvRef[?])                    =>
        val operands = sv match
          case x: IR.SvInst   => x.operands
          case x: IR.SvRef[?] => x.operands
        operands
          .map:
            case w: IR[?] => map(w)
            case w        => w.toString
          .mkString(" ")

    val Context(prefix, debug, types, constants) = compilation.context
    val data = Seq((prefix, "Prefix"), (debug.output, "Debug Symbols"), (types.output, "Type Info"), (constants.output, "Constants")) ++
      compilation.functions
        .zip(compilation.functionBodies)
        .map: (func, body) =>
          (body.body, func.name)

    data.flatMap: (body, title) =>
      val res = body
        .map: ir =>
          val row = ir.name + " " + irInternal(ir)
          map.get(ir) match
            case Some(id) => s"${" ".repeat(5 - id.length)}$id = $row"
            case None     => " ".repeat(8) + row
      s"// $title" :: res
    .foreach(println)
