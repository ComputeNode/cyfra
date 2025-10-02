package io.computenode.cyfra.core.layout

import io.computenode.cyfra.core.binding.{BufferRef, UniformRef}
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}

import scala.annotation.experimental
import scala.compiletime.{error, summonAll}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}
import izumi.reflect.Tag
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.struct.GStructSchema

trait Layout[T]:
  def fromBindings(bindings: Seq[GBinding[?]]): T
  def toBindings(layout: T): Seq[GBinding[?]]
  def layoutRef: T

object Layout:
  inline given derived[T]: Layout[T] = ${ derivedMacro[T] }

  private def derivedMacro[T: Type](using quotes: Quotes): Expr[Layout[T]] =
    import quotes.reflect.*
//    given Printer[Tree] = Printer.TreeShortCode
//    given Printer[Tree] = Printer.TreeCode
    given Printer[Tree] = Printer.TreeStructure

    val layoutType: TypeRepr = TypeRepr.of[T]
    val layoutSymbol: Symbol = layoutType.typeSymbol

    if !layoutSymbol.flags.is(Flags.Case) then
      report.errorAndAbort(s"Can only derive Layout for case classes, tuples and singular GBindings. Found: ${layoutType.show}")

    def generateBindingRef(bindingType: TypeRepr, idx: Int): Expr[GBinding[?]] = bindingType.asType match
      case '[type t <: Value; GBuffer[t]] =>
        val typeStr = TypeRepr.of[t].show
        val fromExpr = Expr.summon[FromExpr[t]] match
          case Some(value) => value
          case None        => report.errorAndAbort(s"Could not find given FromExpr for type $typeStr")
        val tag = Expr.summon[Tag[t]] match
          case Some(value) => value
          case None        => report.errorAndAbort(s"Could not find given Tag for type $typeStr")
        '{ BufferRef[t](${ Expr(idx) })(using ${ tag }, ${ fromExpr }) }
      case '[type t <: GStruct[?]; GUniform[t]] =>
        val typeStr = TypeRepr.of[t].show
        val fromExpr = Expr.summon[FromExpr[t]] match
          case Some(value) => value
          case None        => report.errorAndAbort(s"Could not find given FromExpr for type $typeStr")
        val tag = Expr.summon[Tag[t]] match
          case Some(value) => value
          case None        => report.errorAndAbort(s"Could not find given Tag for type $typeStr")
        val structSchema = Expr.summon[GStructSchema[t]] match
          case Some(value) => value
          case None        => report.errorAndAbort(s"Could not find given GStructSchema for type $typeStr")
        '{ UniformRef[t](${ Expr(idx) })(using ${ tag }, ${ fromExpr }, ${ structSchema }) }
      case _ => report.errorAndAbort(s"All fields of a Layout must be of type GBuffer or GUniform, found: ${bindingType.show}")

    def constructLayout(args: List[Term]): Expr[T] =
      val constructor = Select(New(TypeIdent(layoutSymbol)), layoutSymbol.primaryConstructor)
      val readyConstructor = layoutType.typeArgs match
        case Nil => constructor
        case x   => TypeApply(constructor, x.map(x => TypeTree.of(using x.asType)))
      Apply(readyConstructor, args).asExprOf[T]

//    val s = layoutSymbol.primaryConstructor.paramSymss
//    report.warning(s"Layout: ${layoutSymbol.fullName}, constructor params: ${s.map(_.map(_.tree.show))}")
//
//    val z = '{
//      (BufferRef[Int32](2), UniformRef[GStruct.Empty](3))
//    }

    val fields: List[(String, TypeRepr)] = layoutSymbol.caseFields
      .map(_.tree)
      .map:
        case ValDef(name, tpe, _) => (name, tpe.tpe)
      .map:
        case (name, AppliedType(t, List(arg))) =>
          val resolvedArg =
            if arg.typeSymbol.isTypeParam then
              layoutType.typeArgs
                .find(_.typeSymbol.name == arg.typeSymbol.name)
                .getOrElse(throw new Exception(s"Could not resolve type parameter: ${arg.typeSymbol.name}"))
            else arg
          (name, AppliedType(t, List(arg)))
        case (name, _) => report.errorAndAbort(s"All fields of a Layout must be of type GBuffer or GUniform, found: ${layoutType.show}.$name")

    '{
      new Layout[T] {
        def fromBindings(bindings: Seq[GBinding[?]]): T = ${
          val seq = '{ bindings.toIndexedSeq }

          val args = fields
            .map(_._2)
            .zipWithIndex
            .map: (tpe, idx) =>
              val binding = Apply(Select.unique(seq.asTerm, "apply"), List(Expr(idx).asTerm))
              TypeApply(Select.unique(binding, "asInstanceOf"), List(Inferred(tpe)))

          constructLayout(args)
        }

        def toBindings(layout: T): Seq[GBinding[?]] =
          val result = IndexedSeq.newBuilder[GBinding[?]]
          result.sizeHint(${ Expr(fields.size) })
          ${
            val l = '{ layout }
            val extracted = fields
              .map(_._1)
              .map: name =>
                val binding = Select.unique(l.asTerm, name).asExprOf[GBinding[?]]
                '{ result.addOne(${ binding }) }.asTerm

            val (block, last) =
              val r = extracted.reverse
              (r.tail.reverse, r.head)
            Block(block, last).asExprOf[Any]
          }
          result.result()

        def layoutRef: T = ${
          val buffers = fields.map(_._2).zipWithIndex.map(generateBindingRef).map(_.asTerm)
          constructLayout(buffers)
        }
      }
    }
