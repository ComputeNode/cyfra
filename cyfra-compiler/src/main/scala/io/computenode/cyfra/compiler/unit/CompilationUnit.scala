package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.Compiler.Config
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.core.memory.{BindingRef, FocusRoot, Variable}
import io.computenode.cyfra.utility.Utility.*

case class CompilationUnit(metadata: Metadata, context: Context, functionBodies: List[IRs[?]]):
  def output: List[IR[?]] =
    context.output ++ functionBodies.flatMap(_.body)

object CompilationUnit:
  def apply(functions: List[(FunctionIR[?], IRs[?])], config: Config): CompilationUnit =
    val (f, fir) = functions.unzip
    val context = Context(ExtensionManager(), Nil, Nil, TypeManager(), ConstantsManager(), Nil)
    val meta = Metadata(f, config)
    CompilationUnit(meta, context, fir)

  def debugPrint(compilation: CompilationUnit): Unit =
    var printingError = false

    val irs = compilation.output
    val map = irs
      .collect:
        case ref: RefIR[?] => ref
      .zipWithIndex
      .map(x => (x._1.id, s"%${x._2 + 1}".yellow))
      .toMap

    def irInternal(ir: IR[?]): String = ir match
      case IR.Constant(value)                               => s"($value)"
      case IR.ConstantArgs(value)                           => s"${value.mkString(", ")}"
      case IR.Declare(focus, init)                          => s"#${focus.id}"
      case IR.Read(focus, accessChain)                      => s"#${focus.id} ${accessChain.map(_.id).map(map).mkString(" ")}"
      case IR.Write(focus, accessChain, value)              => s"#${focus.id} ${accessChain.map(_.id).map(map).mkString(" ")} ${map(value.id)}"
      case IR.Operation(func, args)                         => s"${func.name} ${args.map(_.id).map(map).mkString(" ")}"
      case IR.CallWithVar(func, args)                       => s"${func.name} ${args.map(x => s"#${x.id}").mkString(" ")}"
      case IR.CallWithIR(func, args)                        => s"${func.name} ${args.map(x => map(x.id)).mkString(" ")}"
      case IR.Branch(cond, ifTrue, ifFalse, break)          => s"${map(cond.id)} <if body>"
      case IR.Loop(mainBody, continueBody, break, continue) => "<loop body>"
      case IR.Jump(target, value)                           => s"${target.id} ${map(value.id)}"
      case IR.ConditionalJump(cond, target, value)          => s"${map(cond.id)} ${target.id} ${map(value.id)}"
      case IR.Interface(ref)                                => s"${map(ref.id)}"
      case IR.CompositeExtract(value, index)                => s"${map(value.id)} $index"
      case IR.CompositeInsert(original, replacement, index) => s"${map(original.id)} ${map(replacement.id)} $index"
      case IR.CompositeCombine(values)                      => s"${values.map(_.id).map(map).mkString(" ")}"
      case sv: (IR.SvInst | IR.SvRef[?])                    =>
        val operands = sv match
          case x: IR.SvInst   => x.operands
          case x: IR.SvRef[?] => x.tpe.toList ++ x.operands
        operands
          .map:
            case w: RefIR[?] if map.contains(w.id) => map(w.id)
            case w: RefIR[?]                       =>
              printingError = true
              s"(${w.id} NOT FOUND)".redb
            case w: IntWord => w.toString.red
            case w: Text    => s"\"${w.text.green}\""
            case w          => w.toString
          .mkString(" ")

    val Context(extensions, prefix, decorations, types, constants, suffix) = compilation.context
    val data =
      Seq(
        (extensions.output, "Extensions"),
        (prefix, "Prefix"),
        (decorations, "Decorations"),
        (types.output, "Type Info"),
        (constants.output, "Constants"),
        (suffix, "Suffix"),
      ) ++
        compilation.metadata.functions
          .zip(compilation.functionBodies)
          .map: (func, body) =>
            (body.body, func.name)

    data
      .flatMap: (body, title) =>
        val res = body
          .map: ir =>
            val row = ir.name + " " + irInternal(ir)
            ir match
              case r: RefIR[?] =>
                val id =
                  val i = map(r.id)
                  i.substring(5, i.length - 4)
                s"${" ".repeat(5 - id.length)}$id = $row"
              case _ => " ".repeat(8) + row
        s"// $title" :: res
      .foreach(println)
    if printingError then
      println("".red)
      println("Some references were not found in the mapping!".red)
      throw CompilationException("Debug print failed due to missing references")

  extension (focus: FocusRoot[?])
    def id: Int = focus match
      case binding: BindingRef[?] => binding.layoutOffset
      case variable: Variable[?]  => variable.id
      case _                      => ???
