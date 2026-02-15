package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock}
import io.computenode.cyfra.core.expression.BuildInFunction.{BuildInFunction0, BuildInFunction1, BuildInFunction2, BuildInFunction3, BuildInFunction4}
import io.computenode.cyfra.utility.cats.Monad
import izumi.reflect.{Tag, TagK}

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type, Varargs}

trait Value[A]:
  protected def extractUnsafe(ir: ExpressionBlock[A]): A
  def tag: Tag[A]
  def baseTag: Option[TagK[?]]
  def composite: List[Value[?]]

  final def indirect(ir: Expression[A]): A = extract(ExpressionBlock(ir, List()))
  final def extract(block: ExpressionBlock[A]): A =
    if !block.isPure then throw RuntimeException("Cannot embed impure expression")
    extractUnsafe(block)
  final def peel(x: A): ExpressionBlock[A] =
    summon[Monad[ExpressionBlock]].pure(x)
  @tailrec
  final def bottomComposite: Value[?] =
    composite match
      case List(c) => c.bottomComposite
      case _       => this

object Value:
  def apply[A](using v: Value[A]): Value[A] = v

  trait Scalar[A] extends Value[A]:
    def baseTag: Option[TagK[?]] = None
    def composite: List[Value[?]] = Nil

  def map[Res: Value](f: BuildInFunction0): Res =
    val next = Expression.BuildInOperation[Res](f, Nil)
    Value[Res].extract(ExpressionBlock(next, List(next)))

  def map[A1: Value, Res: Value](x1: A1)(f: BuildInFunction1): Res =
    val arg1 = Value[A1].peel(x1)
    val next = Expression.BuildInOperation[Res](f, List(arg1.result))
    Value[Res].extract(arg1.add(next))

  def map[A1: Value, A2: Value, Res: Value](x1: A1, x2: A2)(f: BuildInFunction2): Res =
    val arg1 = Value[A1].peel(x1)
    val arg2 = Value[A2].peel(x2)
    val next = Expression.BuildInOperation[Res](f, List(arg1.result, arg2.result))
    Value[Res].extract(arg1.extend(arg2).add(next))

  def map[A1: Value, A2: Value, A3: Value, Res: Value](x1: A1, x2: A2, x3: A3)(f: BuildInFunction3): Res =
    val arg1 = Value[A1].peel(x1)
    val arg2 = Value[A2].peel(x2)
    val arg3 = Value[A3].peel(x3)
    val next = Expression.BuildInOperation[Res](f, List(arg1.result, arg2.result, arg3.result))
    Value[Res].extract(arg1.extend(arg2).extend(arg3).add(next))

  def map[A1: Value, A2: Value, A3: Value, A4: Value, Res: Value](x1: A1, x2: A2, x3: A3, x4: A4)(f: BuildInFunction4): Res =
    val arg1 = Value[A1].peel(x1)
    val arg2 = Value[A2].peel(x2)
    val arg3 = Value[A3].peel(x3)
    val arg4 = Value[A4].peel(x4)
    val next = Expression.BuildInOperation[Res](f, List(arg1.result, arg2.result, arg3.result, arg4.result))
    Value[Res].extract(arg1.extend(arg2).extend(arg3).extend(arg4).add(next))

  extension [A: Value](x: A) def irs: ExpressionBlock[A] = Value[A].peel(x)

  // Derived Value implementation for tuples/products
  class Derived[T](elemValues: List[Value[?]], theTag: Tag[T], theBaseTag: Option[TagK[?]], extract: (ExpressionBlock[T], Value[T]) => T)
      extends Value[T]:
    protected def extractUnsafe(ir: ExpressionBlock[T]): T = extract(ir, this)
    def tag: Tag[T] = theTag
    def baseTag: Option[TagK[?]] = theBaseTag
    def composite: List[Value[?]] = elemValues

  // Runtime helper for extraction - used by the macro
  def extractComposite[Parent, T](ir: ExpressionBlock[Parent], parentValue: Value[Parent], elemValue: Value[T], idx: Int): T =
    val expr = Expression.Composite[Parent & Tuple, idx.type](ir.result.asInstanceOf[Expression[Parent & Tuple]], idx)(using
      parentValue.asInstanceOf[Value[Parent & Tuple]],
    )
    elemValue.extract(ir.add(expr.asInstanceOf[Expression[T]]))

  // Helper to get Tuple base tag - avoids compile-time kind issues
  private[expression] val tupleBaseTag: Option[TagK[?]] = Some(Tag[Tuple].asInstanceOf[TagK[?]])

  // Auto-derivation for tuples and case classes
  inline given derived[T]: Value[T] = ${ derivedMacro[T] }

  private def derivedMacro[T: Type](using quotes: Quotes): Expr[Value[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    if !sym.flags.is(Flags.Case) then report.errorAndAbort(s"Can only derive Value for case classes and tuples. Found: ${tpe.show}")

    // Get element types from tuple/case class
    val elemTypes: List[TypeRepr] = tpe match
      case AppliedType(_, args) => args
      case _                    => sym.caseFields.map(f => tpe.memberType(f))

    // Generate Value lookups for each element
    def lookupValue(elemType: TypeRepr): Expr[Value[?]] =
      val valueType = TypeRepr.of[Value].appliedTo(elemType)
      Implicits.search(valueType) match
        case iss: ImplicitSearchSuccess => iss.tree.asExprOf[Value[?]]
        case isf: ImplicitSearchFailure =>
          report.errorAndAbort(s"Could not find Value[${elemType.show}]: ${isf.explanation}")

    val elemValueExprs: List[Expr[Value[?]]] = elemTypes.map(lookupValue)
    val elemValuesExpr: Expr[List[Value[?]]] = Expr.ofList(elemValueExprs)

    // Get Tag[T]
    val tagExpr: Expr[Tag[T]] = Implicits.search(TypeRepr.of[Tag[T]]) match
      case iss: ImplicitSearchSuccess => iss.tree.asExprOf[Tag[T]]
      case isf: ImplicitSearchFailure =>
        report.errorAndAbort(s"Could not find Tag[${tpe.show}]: ${isf.explanation}")

    // Get baseTag for tuples
    val isTuple = tpe match
      case AppliedType(tycon, _) => tycon.typeSymbol.fullName.startsWith("scala.Tuple")
      case _                     => false
    val baseTagExpr: Expr[Option[TagK[?]]] =
      if isTuple then '{ Value.tupleBaseTag } else '{ None }

    // Generate tuple construction from array
    def constructFromArray(arrExpr: Expr[Array[Any]]): Expr[T] =
      val args = elemTypes.zipWithIndex.map: (elemType, idx) =>
        elemType.asType match
          case '[t] =>
            val elem = '{ $arrExpr(${ Expr(idx) }).asInstanceOf[t] }
            elem.asTerm

      val constructor = Select(New(TypeIdent(sym)), sym.primaryConstructor)
      val applied = tpe.typeArgs match
        case Nil      => constructor
        case typeArgs => TypeApply(constructor, typeArgs.map(t => TypeTree.of(using t.asType)))

      Apply(applied, args).asExprOf[T]

    // Generate extraction lambda
    val extractLambda: Expr[(ExpressionBlock[T], Value[T]) => T] =
      '{ (ir: ExpressionBlock[T], self: Value[T]) =>
        val elements = Array[Any](${
          val extractions = elemTypes.zipWithIndex.map: (elemType, idx) =>
            elemType.asType match
              case '[t] =>
                val valueExpr = elemValueExprs(idx).asExprOf[Value[t]]
                '{ Value.extractComposite[T, t](ir, self, $valueExpr, ${ Expr(idx) }) }
          Varargs(extractions)
        }*)
        ${ constructFromArray('elements) }
      }

    '{ new Value.Derived[T]($elemValuesExpr, $tagExpr, $baseTagExpr, $extractLambda) }
