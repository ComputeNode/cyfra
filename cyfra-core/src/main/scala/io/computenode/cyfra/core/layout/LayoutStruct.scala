package io.computenode.cyfra.core.layout

import io.computenode.cyfra.core.buffer.BufferRef
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.buffer.GBuffer
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

    if !fieldTypes.forall(_ <:< TypeRepr.of[GBuffer[?]]) then
      report.errorAndAbort("LayoutStruct can only be derived for case classes with GBuffer elements")

    val valueTypes = fieldTypes.map(_.typeArgs.headOption.getOrElse(report.errorAndAbort("GBuffer must have a value type")))

    // summon izumi tags
    val tags = valueTypes.map: tpe =>
      tpe.asType match
        case '[t] =>
          (
            tpe.asType,
            Expr.summon[Tag[t]] match
              case Some(tagExpr) => tagExpr
              case None          => report.errorAndAbort(s"Cannot summon Tag for type ${tpe.show}"),
          )

    val buffers = tags.zipWithIndex.map:
      case ((tpe, tag), i) =>
        tpe match
          case '[type t <: Value; t] =>
            '{
              BufferRef[t](${ Expr(i) }, ${ tag.asExprOf[Tag[t]] })
            }

    val constructor = sym.primaryConstructor

    val layoutInstance = Apply(Select(New(TypeIdent(sym)), constructor), buffers.map(_.asTerm))

    val layoutRef = layoutInstance.asExprOf[T]

    val soleTags = tags.map(_._2.asExprOf[Tag[? <: Value]]).toList

    '{
      LayoutStruct[T]($layoutRef, ${ Expr.ofList(soleTags) })
    }
