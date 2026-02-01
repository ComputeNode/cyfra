package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.{IntegerType, Mat, RuntimeArray, Vec}

import scala.quoted.{Expr, Quotes, Type}

sealed trait Focus[T: Value]:
  def v: Value[T] = Value[T]
  def getRoot: FocusRoot[?]

trait FocusRoot[T: Value] extends Focus[T]:
  def getRoot: FocusRoot[?] = this

case class FocusConstant[Parent: Value, T: Value](parent: Focus[Parent], value: Int) extends Focus[T]:
  def getRoot: FocusRoot[?] = parent.getRoot

case class FocusDynamic[Parent: Value, T: Value](parent: Focus[Parent], value: IntegerType) extends Focus[T]:
  def getRoot: FocusRoot[?] = parent.getRoot

object Focus:
  trait FocusContext:
    extension [To: Value](from: RuntimeArray[To])
      def at(index: Int): To = scala.sys.error("method can only be used inside focus lambda")

      def at[I <: IntegerType: Value](index: I): To = scala.sys.error("method can only be used inside focus lambda")

  extension [From: Value, To: Value](from: Focus[From])
    transparent inline def focus(inline lambda: FocusContext ?=> From => To): Focus[To] =
      ${ focusImpl[From, To]('from, 'lambda) }

  private def focusImpl[From: Type, To: Type](from: Expr[Focus[From]], lambda: Expr[FocusContext ?=> From => To])(using
    quotes: Quotes,
  ): Expr[Focus[To]] =
    import quotes.reflect.*

    // Represents an access step in the focus path
    enum AccessStep:
      case TupleElement(index: Int, parentType: TypeRepr, elementType: TypeRepr)
      case CaseClassField(fieldName: String, index: Int, parentType: TypeRepr, elementType: TypeRepr)
      case ArrayConstant(index: Int, elementType: TypeRepr)
      case ArrayDynamic(indexExpr: Term, indexType: TypeRepr, elementType: TypeRepr)

    // Extract lambda body - handles context function wrapping
    def extractLambdaBody(term: Term): Term = term match
      case Inlined(_, _, body)                         => extractLambdaBody(body)
      case Block(List(DefDef(_, _, _, Some(body))), _) => extractLambdaBody(body)
      case Block(Nil, body)                            => extractLambdaBody(body)
      case Lambda(_, body)                             => body
      case _                                           => term

    // Get the tuple element index from method name like "_1", "_2", etc.
    def tupleElementIndex(name: String): Option[Int] =
      if name.startsWith("_") then name.drop(1).toIntOption else None

    // Recursively collect access steps from the expression tree
    // Returns (steps from outer to inner, the parameter identifier)
    def collectSteps(term: Term): (List[AccessStep], Term) = term match
      // Handle inlined expressions
      case Inlined(_, _, body) =>
        collectSteps(body)

      // Tuple element access: expr._N
      case Select(qualifier, name) if tupleElementIndex(name).isDefined =>
        val index = tupleElementIndex(name).get
        val (innerSteps, param) = collectSteps(qualifier)
        val step = AccessStep.TupleElement(index, qualifier.tpe.widen, term.tpe.widen)
        (innerSteps :+ step, param)

      // Case class field access: expr.fieldName
      case Select(qualifier, fieldName) =>
        val qualType = qualifier.tpe.widen
        val qualSym = qualType.typeSymbol
        if qualSym.flags.is(Flags.Case) then
          val fields = qualSym.caseFields
          val fieldIndex = fields.indexWhere(_.name == fieldName)
          if fieldIndex >= 0 then
            val (innerSteps, param) = collectSteps(qualifier)
            // Case class fields are 1-indexed like tuples for consistency with FocusConstant
            val step = AccessStep.CaseClassField(fieldName, fieldIndex + 1, qualType, term.tpe.widen)
            (innerSteps :+ step, param)
          else report.errorAndAbort(s"Field '$fieldName' not found in case class ${qualType.show}")
        else report.errorAndAbort(s"Cannot access field '$fieldName' on non-case-class type ${qualType.show}")

      // Extension method array access with constant Int: context.at[Elem](qualifier)(constIndex)(evidence)
      // Tree: Apply(Apply(Apply(TypeApply(Select(context, "at"), List(elemType)), List(qualifier)), List(index)), List(evidence))
      case Apply(Apply(Apply(TypeApply(Select(_, "at"), List(elemTypeTree)), List(qualifier)), List(indexTerm)), _)
          if indexTerm.tpe.widen <:< TypeRepr.of[Int] =>
        indexTerm match
          case Literal(IntConstant(idx)) =>
            val (innerSteps, param) = collectSteps(qualifier)
            val elemType = elemTypeTree.tpe.widen
            val step = AccessStep.ArrayConstant(idx, elemType)
            (innerSteps :+ step, param)
          case _ =>
            report.errorAndAbort(s"Expected constant Int index in at(), got: ${indexTerm.show}")

      // Extension method array access with dynamic IntegerType: context.at[Elem](qualifier)[I](index)(evidences)
      // Tree: Apply(Apply(TypeApply(Apply(TypeApply(Select(context, "at"), List(elemType)), List(qualifier)), List(indexType)), List(index)), List(evidences))
      case Apply(Apply(TypeApply(Apply(TypeApply(Select(_, "at"), List(elemTypeTree)), List(qualifier)), _), List(indexTerm)), _) =>
        val (innerSteps, param) = collectSteps(qualifier)
        val elemType = elemTypeTree.tpe.widen
        val step = AccessStep.ArrayDynamic(indexTerm, indexTerm.tpe.widen, elemType)
        (innerSteps :+ step, param)

      // Base case: the lambda parameter
      case ident: Ident =>
        (Nil, ident)

      case other =>
        report.errorAndAbort(s"Unsupported focus expression: ${other.show}\nTree: $other")

    // Build the Focus expression from collected steps
    def buildFocusExpr(steps: List[AccessStep], currentFocus: Term, currentType: TypeRepr): Term =
      steps match
        case Nil          => currentFocus
        case step :: rest =>
          step match
            case AccessStep.TupleElement(index, parentType, elementType) =>
              val parentTypeTree = TypeTree.of(using parentType.asType)
              val elementTypeTree = TypeTree.of(using elementType.asType)

              // Find Value instances for parent and element types
              val parentValue = Implicits.search(TypeRepr.of[Value].appliedTo(parentType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${parentType.show}")

              val elementValue = Implicits.search(TypeRepr.of[Value].appliedTo(elementType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${elementType.show}")

              val focusConstantType = TypeRepr.of[FocusConstant].appliedTo(List(parentType, elementType))
              val focusConstantCompanion = Ref(Symbol.requiredModule("io.computenode.cyfra.core.memory.FocusConstant"))

              val newFocus = Apply(
                Apply(
                  TypeApply(Select.unique(focusConstantCompanion, "apply"), List(parentTypeTree, elementTypeTree)),
                  List(currentFocus, Literal(IntConstant(index))),
                ),
                List(parentValue, elementValue),
              )
              buildFocusExpr(rest, newFocus, elementType)

            case AccessStep.CaseClassField(fieldName, index, parentType, elementType) =>
              val parentTypeTree = TypeTree.of(using parentType.asType)
              val elementTypeTree = TypeTree.of(using elementType.asType)

              // Find Value instances for parent and element types
              val parentValue = Implicits.search(TypeRepr.of[Value].appliedTo(parentType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${parentType.show}")

              val elementValue = Implicits.search(TypeRepr.of[Value].appliedTo(elementType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${elementType.show}")

              val focusConstantCompanion = Ref(Symbol.requiredModule("io.computenode.cyfra.core.memory.FocusConstant"))

              val newFocus = Apply(
                Apply(
                  TypeApply(Select.unique(focusConstantCompanion, "apply"), List(parentTypeTree, elementTypeTree)),
                  List(currentFocus, Literal(IntConstant(index))),
                ),
                List(parentValue, elementValue),
              )
              buildFocusExpr(rest, newFocus, elementType)

            case AccessStep.ArrayConstant(index, elementType) =>
              val parentType = TypeRepr.of[RuntimeArray].appliedTo(elementType)
              val parentTypeTree = TypeTree.of(using parentType.asType)
              val elementTypeTree = TypeTree.of(using elementType.asType)

              val parentValue = Implicits.search(TypeRepr.of[Value].appliedTo(parentType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${parentType.show}")

              val elementValue = Implicits.search(TypeRepr.of[Value].appliedTo(elementType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${elementType.show}")

              val focusConstantCompanion = Ref(Symbol.requiredModule("io.computenode.cyfra.core.memory.FocusConstant"))

              val newFocus = Apply(
                Apply(
                  TypeApply(Select.unique(focusConstantCompanion, "apply"), List(parentTypeTree, elementTypeTree)),
                  List(currentFocus, Literal(IntConstant(index))),
                ),
                List(parentValue, elementValue),
              )
              buildFocusExpr(rest, newFocus, elementType)

            case AccessStep.ArrayDynamic(indexExpr, indexType, elementType) =>
              val parentType = TypeRepr.of[RuntimeArray].appliedTo(elementType)
              val parentTypeTree = TypeTree.of(using parentType.asType)
              val elementTypeTree = TypeTree.of(using elementType.asType)

              val parentValue = Implicits.search(TypeRepr.of[Value].appliedTo(parentType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${parentType.show}")

              val elementValue = Implicits.search(TypeRepr.of[Value].appliedTo(elementType)) match
                case success: ImplicitSearchSuccess => success.tree
                case _                              => report.errorAndAbort(s"Could not find Value instance for ${elementType.show}")

              val focusDynamicCompanion = Ref(Symbol.requiredModule("io.computenode.cyfra.core.memory.FocusDynamic"))

              // Cast the index expression to IntegerType
              val indexAsIntegerType = indexExpr.asExprOf[IntegerType].asTerm

              val newFocus = Apply(
                Apply(
                  TypeApply(Select.unique(focusDynamicCompanion, "apply"), List(parentTypeTree, elementTypeTree)),
                  List(currentFocus, indexAsIntegerType),
                ),
                List(parentValue, elementValue),
              )
              buildFocusExpr(rest, newFocus, elementType)

    val lambdaBody = extractLambdaBody(lambda.asTerm)
    val (steps, _) = collectSteps(lambdaBody)

    val result = buildFocusExpr(steps, from.asTerm, TypeRepr.of[From])
    result.asExprOf[Focus[To]]
