package io.computenode.cyfra.dsl.direct

import io.computenode.cyfra.core.{ExpressionProgram, GProgram, Layout}
import io.computenode.cyfra.core.GProgram.{InitProgramLayout, ProgramDispatch, WorkDimensions}
import io.computenode.cyfra.core.expression.{Operator, CustomFunction, Expression, ExpressionBlock, ExpressionHolder, JumpTarget, Value, given}
import io.computenode.cyfra.core.expression.CustomFunction.CustomFunction1
import io.computenode.cyfra.core.memory.{Focus, FocusConstant, FocusDynamic, FocusRoot, GBuffer, GUniform, LocalVariable, Variable}
import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.core.expression.Value.irs
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.utility.cats.Monad

class GIO:
  private var result: List[Expression[?]] = Nil
  private[direct] def extend(irs: List[Expression[?]]): Unit = result = irs ++ result
  private[direct] def add(ir: Expression[?]): Unit = result = ir :: result
  private[direct] def getResult: List[Expression[?]] = result

object GIO:
  def reify[T: Value](body: GIO ?=> T): ExpressionBlock[T] =
    val gio = new GIO()
    val v = body(using gio).irs
    val irs = gio.getResult
    ExpressionBlock(v.result, v.body ++ irs)

  def reflect[A: Value](res: ExpressionBlock[A])(using gio: GIO): A =
    gio.extend(res.body)
    Value[A].indirect(res.result)

  def declare[T: Value](init: Option[T] = None)(using gio: GIO): LocalVariable[T] =
    val v = LocalVariable[T]()
    val exp = init match
      case Some(value) =>
        val irs = value.irs
        gio.extend(irs.body)
        Some(irs.result)
      case None => None
    gio.add(Expression.VariableDeclare(v, exp))
    v

  def read[T: Value](focus: Focus[T])(using gio: GIO): T =
    val accessChain = extractFocusTrail(focus)
    val read = Expression.Read(focus.getRoot, accessChain.map(_.result))
    gio.extend(read :: accessChain.flatMap(_.body))
    Value[T].indirect(read)

  def write[T: Value](focus: Focus[T], value: T)(using gio: GIO): Unit =
    val accessChain = extractFocusTrail(focus)
    val v = value.irs
    val write = Expression.Write(focus.getRoot, accessChain.map(_.result), v.result)
    gio.extend(write :: accessChain.flatMap(_.body) ::: v.body)

  private def extractFocusTrail(focus: Focus[?]): List[ExpressionBlock[?]] =
    def extractFocusTrailAcc(focus: Focus[?]): List[ExpressionBlock[?]] =
      focus match
        case root: FocusRoot[?]           => Nil
        case FocusConstant(parent, value) => ExpressionBlock(Expression.Constant[Int32](value)) :: extractFocusTrail(parent)
        case FocusDynamic(parent, value)  => value.asInstanceOf[ExpressionHolder[?]].block :: extractFocusTrail(parent)

    extractFocusTrailAcc(focus).reverse

  def op[Res: Value](func: Operator.Operator0)(using gio: GIO): Res =
    val next = Expression.Operation[Res](func, List())
    gio.add(next)
    Value[Res].indirect(next)

  def op[A: Value, Res: Value](func: Operator.Operator1, arg: A)(using gio: GIO): Res =
    val a = arg.irs
    val next = Expression.Operation[Res](func, List(a.result))
    gio.extend(next :: a.body)
    Value[Res].indirect(next)

  def op[A1: Value, A2: Value, Res: Value](func: Operator.Operator2, arg1: A1, arg2: A2)(using gio: GIO): Res =
    val a1 = arg1.irs
    val a2 = arg2.irs
    val next = Expression.Operation[Res](func, List(a1.result, a2.result))
    gio.extend(next :: a1.body ++ a2.body)
    Value[Res].indirect(next)

  def op[A1: Value, A2: Value, A3: Value, Res: Value](func: Operator.Operator3, arg1: A1, arg2: A2, arg3: A3)(using gio: GIO): Res =
    val a1 = arg1.irs
    val a2 = arg2.irs
    val a3 = arg3.irs
    val next = Expression.Operation[Res](func, List(a1.result, a2.result, a3.result))
    gio.extend(next :: a1.body ++ a2.body ++ a3.body)
    Value[Res].indirect(next)

  def op[A1: Value, A2: Value, A3: Value, A4: Value, Res: Value](func: Operator.Operator4, arg1: A1, arg2: A2, arg3: A3, arg4: A4)(using
    gio: GIO,
  ): Res =
    val a1 = arg1.irs
    val a2 = arg2.irs
    val a3 = arg3.irs
    val a4 = arg4.irs
    val next = Expression.Operation[Res](func, List(a1.result, a2.result, a3.result, a4.result))
    gio.extend(next :: a1.body ++ a2.body ++ a3.body ++ a4.body)
    Value[Res].indirect(next)

  def call[A: Value, Res: Value](func: CustomFunction1[Res, A], arg: Variable[A])(using gio: GIO): Res =
    val next = Expression.CustomCall(func, List(arg))
    gio.add(next)
    Value[Res].indirect(next)

  def branch[T: Value](cond: Bool, ifTrue: (JumpTarget[T], GIO) ?=> T, ifFalse: (JumpTarget[T], GIO) ?=> T)(using gio: GIO): T =
    val c = cond.irs
    val jt = JumpTarget[T]()
    val t = GIO.reify(ifTrue(using jt))
    val f = GIO.reify(ifFalse(using jt))
    val branch = Expression.Branch(c.result, t, f, jt)
    gio.extend(branch :: c.body)
    Value[T].indirect(branch)

  def loop(mainBody: (BreakTarget, ContinueTarget, GIO) ?=> Unit, continueBody: GIO ?=> Unit)(using gio: GIO): Unit =
    val jb = BreakTarget()
    val jc = ContinueTarget()
    val m = GIO.reify(mainBody(using jb, jc))
    val c = GIO.reify(continueBody)
    val loop = Expression.Loop(m, c, jb, jc)
    gio.add(loop)

  def conditionalJump[T: Value](cond: Bool, value: T)(using target: JumpTarget[T], gio: GIO): Unit =
    val c = cond.irs
    val v = value.irs
    val cj = Expression.ConditionalJump(c.result, target, v.result)
    gio.extend(cj :: c.body ++ v.body)

  def jump[T: Value](value: T)(using target: JumpTarget[T], gio: GIO): Unit =
    val v = value.irs
    val j = Expression.Jump(target, v.result)
    gio.extend(j :: v.body)

  def break(using target: BreakTarget, gio: GIO): Unit =
    jump(())

  def conditionalBreak(cond: Bool)(using target: BreakTarget, gio: GIO): Unit =
    conditionalJump(cond, ())

  def continue(using target: ContinueTarget, gio: GIO): Unit =
    jump(())

  def conditionalContinue(cond: Bool)(using target: ContinueTarget, gio: GIO): Unit =
    conditionalJump(cond, ())
