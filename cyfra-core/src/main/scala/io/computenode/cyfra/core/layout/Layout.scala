package io.computenode.cyfra.core.layout

import io.computenode.cyfra.core.binding.{BufferRef, GBinding, GBuffer, GUniform, UniformRef}
import io.computenode.cyfra.core.expression.*

import scala.annotation.experimental
import scala.compiletime.{error, summonAll}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}
import izumi.reflect.Tag

trait Layout[T]:
  def fromBindings(bindings: Seq[GBinding[?]]): T
  def toBindings(layout: T): Seq[GBinding[?]]
  def layoutRef: T

object Layout:
  def apply[T](using layout: Layout[T]): Layout[T] = layout

  inline given derived[T]: Layout[T] = ${ derivedMacro[T] }

  private def derivedMacro[T: Type](using quotes: Quotes): Expr[Layout[T]] =
    import quotes.reflect.*

    val layoutType: TypeRepr = TypeRepr.of[T]
    val layoutSymbol: Symbol = layoutType.typeSymbol

    if !layoutSymbol.flags.is(Flags.Case) then
      report.errorAndAbort(s"Can only derive Layout for case classes, tuples and singular GBindings. Found: ${layoutType.show}")

    def generateBindingRef(bindingType: TypeRepr, idx: Int): Expr[GBinding[?]] = bindingType.asType match
      case '[type t; GBuffer[t]] =>
        val value = Implicits.search(TypeRepr.of[Value[t]]) match
          case iss: ImplicitSearchSuccess => iss.tree.asExprOf[Value[t]]
          case isf: ImplicitSearchFailure => report.errorAndAbort(s"Could not find Value[${TypeRepr.of[t].show}]: ${isf.explanation}")
        '{ BufferRef[t](${ Expr(idx) })(using $value) }
      case '[type t; GUniform[t]] =>
        val value = Implicits.search(TypeRepr.of[Value[t]]) match
          case iss: ImplicitSearchSuccess => iss.tree.asExprOf[Value[t]]
          case isf: ImplicitSearchFailure => report.errorAndAbort(s"Could not find Value[${TypeRepr.of[t].show}]: ${isf.explanation}")
        '{ UniformRef[t](${ Expr(idx) })(using $value) }
      case _ => report.errorAndAbort(s"All fields of a Layout must be of type GBuffer or GUniform, found: ${bindingType.show}")

    def constructLayout(args: List[Term]): Expr[T] =
      val constructor = Select(New(TypeIdent(layoutSymbol)), layoutSymbol.primaryConstructor)
      val readyConstructor = layoutType.typeArgs match
        case Nil => constructor
        case x   => TypeApply(constructor, x.map(x => TypeTree.of(using x.asType)))

      val paramSymss = layoutSymbol.primaryConstructor.paramSymss

      val hasImplicits = paramSymss.size > 2

      if hasImplicits then
        val regularParams = paramSymss(1)
        val implicitParams = paramSymss(2)

        val implicitArgs = implicitParams.map: param =>
          val paramType = param.tree match
            case ValDef(_, tpt, _) => tpt.tpe

          Implicits.search(paramType) match
            case iss: ImplicitSearchSuccess => iss.tree
            case isf: ImplicitSearchFailure =>
              report.errorAndAbort(s"Could not find implicit ${param.name} of type ${paramType.show}: ${isf.explanation}")

        Apply(Apply(readyConstructor, args), implicitArgs).asExprOf[T]
      else Apply(readyConstructor, args).asExprOf[T]

    val fields: List[(String, TypeRepr)] = layoutSymbol.caseFields
      .map(_.tree)
      .map:
        case ValDef(name, tpe, _) => (name, tpe.tpe)
      .zipWithIndex
      .map:
        case ((name, tpe), idx) =>
          val resolvedType =
            if tpe.typeSymbol.isTypeParam then
              layoutType.typeArgs
                .find(_.typeSymbol.name == tpe.typeSymbol.name)
                .orElse(if idx < layoutType.typeArgs.size then Some(layoutType.typeArgs(idx)) else None)
                .getOrElse(tpe)
            else tpe

          resolvedType match
            case AppliedType(t, List(arg)) =>
              val resolvedArg =
                if arg.typeSymbol.isTypeParam then
                  layoutType.typeArgs
                    .find(_.typeSymbol.name == arg.typeSymbol.name)
                    .getOrElse(arg)
                else arg
              (name, AppliedType(t, List(resolvedArg)))
            case _ =>
              report.errorAndAbort(
                s"All fields of a Layout must be of type GBuffer or GUniform, found: ${layoutType.show}.$name (resolved to: ${resolvedType.show})",
              )

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
