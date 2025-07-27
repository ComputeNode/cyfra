package io.computenode.cyfra.core.layout

import io.computenode.cyfra.core.binding.{BufferRef, UniformRef}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.compiletime.{error, summonAll}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

case class LayoutStruct[T <: Layout: Tag](private[cyfra] val layoutRef: T, private[cyfra] val elementTypes: List[Tag[? <: Value]])

object LayoutStruct:

  inline given derived[T <: Layout: Tag]: LayoutStruct[T] = ${ derivedImpl }

  def derivedImpl[T <: Layout: Type](using quotes: Quotes): Expr[LayoutStruct[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    if !sym.isClassDef || !sym.flags.is(Flags.Case) then report.errorAndAbort("LayoutStruct can only be derived for case classes")

    val fieldTypes = sym.caseFields
      .map(_.tree)
      .map:
        case ValDef(_, tpt, _) => tpt.tpe
        case _                 => report.errorAndAbort("Unexpected field type in case class")

    if !fieldTypes.forall(_ <:< TypeRepr.of[GBinding[?]]) then
      report.errorAndAbort("LayoutStruct can only be derived for case classes with GBinding elements")

    val valueTypes = fieldTypes.map: ftype =>
      (ftype, ftype.typeArgs.headOption.getOrElse(report.errorAndAbort("GBuffer must have a value type")))

    // summon izumi tags
    val typeGivens = valueTypes.map:
      case (ftype, farg) =>
        farg.asType match
          case '[type t <: Value; t] =>
            (
              ftype.asType,
              farg.asType,
              Expr.summon[Tag[t]] match
                case Some(tagExpr) => tagExpr
                case None          => report.errorAndAbort(s"Cannot summon Tag for type ${tpe.show}"),
              Expr.summon[FromExpr[t]] match
                case Some(fromExpr) => fromExpr
                case None           => report.errorAndAbort(s"Cannot summon FromExpr for type ${tpe.show}"),
            )

    val buffers = typeGivens.zipWithIndex.map:
      case ((ftype, tpe, tag, fromExpr), i) =>
        tpe match
          case '[type t <: Value; t] =>
            ftype match
              case '[type tg <: GBuffer[?]; tg] =>
                '{
                  BufferRef[t](${ Expr(i) }, ${ tag.asExprOf[Tag[t]] })(using ${ tag.asExprOf[Tag[t]] }, ${ fromExpr.asExprOf[FromExpr[t]] })
                }
              case '[type tg <: GUniform[?]; tg] =>
                '{
                  UniformRef[t](${ Expr(i) }, ${ tag.asExprOf[Tag[t]] })(using ${ tag.asExprOf[Tag[t]] }, ${ fromExpr.asExprOf[FromExpr[t]] })
                }

    val constructor = sym.primaryConstructor

    val layoutInstance = Apply(Select(New(TypeIdent(sym)), constructor), buffers.map(_.asTerm))

    val layoutRef = layoutInstance.asExprOf[T]

    val soleTags = typeGivens.map(_._3.asExprOf[Tag[? <: Value]]).toList

    '{
      LayoutStruct[T]($layoutRef, ${ Expr.ofList(soleTags) })
    }
