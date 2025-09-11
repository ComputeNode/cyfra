package io.computenode.cyfra.core.layout

import io.computenode.cyfra.core.binding.{BufferRef, UniformRef}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
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
      ftype match
        case AppliedType(_, args) if args.nonEmpty =>
          val valueType = args.head
          // Ensure we're working with the original type parameter, not the instance type
          val resolvedType = valueType match
            case tr if tr.typeSymbol.isTypeParam =>
              // Find the corresponding type parameter from the original class
              tpe.typeArgs.find(_.typeSymbol.name == tr.typeSymbol.name).getOrElse(tr)
            case tr => tr
          (ftype, resolvedType)
        case _ =>
          report.errorAndAbort("GBinding must have a value type")

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
                case None          => report.errorAndAbort(s"Cannot summon Tag for type ${farg.show}"),
              Expr.summon[FromExpr[t]] match
                case Some(fromExpr) => fromExpr
                case None           => report.errorAndAbort(s"Cannot summon FromExpr for type ${farg.show}"),
            )

    val buffers = typeGivens.zipWithIndex.map:
      case ((ftype, tpe, tag, fromExpr), i) =>
        (tpe, ftype) match
          case ('[type t <: Value; t], '[type tg <: GBuffer[?]; tg]) =>
            '{
              BufferRef[t](${ Expr(i) }, ${ tag.asExprOf[Tag[t]] })(using ${ tag.asExprOf[Tag[t]] }, ${ fromExpr.asExprOf[FromExpr[t]] })
            }
          case ('[type t <: GStruct[?]; t], '[type tg <: GUniform[?]; tg]) =>
            val structSchema = Expr.summon[GStructSchema[t]] match
              case Some(s) => s
              case None    => report.errorAndAbort(s"Cannot summon GStructSchema for type")
            '{
              UniformRef[t](${ Expr(i) }, ${ tag.asExprOf[Tag[t]] })(using ${ tag.asExprOf[Tag[t]] }, ${ fromExpr.asExprOf[FromExpr[t]] }, ${ structSchema })
            }

    val constructor = sym.primaryConstructor
    report.info(s"Constructor: ${constructor.fullName} with params ${constructor.paramSymss.flatten.map(_.name).mkString(", ")}")

    val typeArgs = tpe.typeArgs

    val layoutInstance =
      if (typeArgs.isEmpty) then
        Apply(Select(New(TypeIdent(sym)), constructor), buffers.map(_.asTerm))
      else
        Apply(
          TypeApply(
            Select(New(TypeIdent(sym)), constructor),
            typeArgs.map(arg => TypeTree.of(using arg.asType))
          ),
          buffers.map(_.asTerm)
        )

    val layoutRef = layoutInstance.asExprOf[T]

    val soleTags = typeGivens.map(_._3.asExprOf[Tag[? <: Value]]).toList

    '{
      LayoutStruct[T]($layoutRef, ${ Expr.ofList(soleTags) })
    }
