package io.computenode.cyfra.dsl.direct

import io.computenode.cyfra.core.expression.{
  Bool,
  unitZero,
  BuildInFunction,
  CustomFunction,
  Expression,
  ExpressionBlock,
  JumpTarget,
  UInt32,
  Value,
  Var,
  given,
}
import io.computenode.cyfra.core.binding.GBuffer
import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.core.expression.Value.irs

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
    summon[Value[A]].indirect(res.result)

  def read[T: Value](buffer: GBuffer[T], index: UInt32)(using gio: GIO): T =
    val idx = index.irs
    val read = Expression.ReadBuffer(buffer, idx.result)
    gio.extend(read :: idx.body)
    Value[T].indirect(read)

  def write[T: Value](buffer: GBuffer[T], index: UInt32, value: T)(using gio: GIO): Unit =
    val idx = index.irs
    val v = value.irs
    val write = Expression.WriteBuffer(buffer, idx.result, v.result)
    gio.extend(write :: idx.body ++ v.body)

  def declare[T: Value]()(using gio: GIO): Var[T] =
    val variable = Var[T]()
    gio.add(Expression.VarDeclare(variable))
    variable

  def read[T: Value](variable: Var[T])(using gio: GIO): T =
    val read = Expression.VarRead(variable)
    gio.add(read)
    Value[T].indirect(read)

  def write[T: Value](variable: Var[T], value: T)(using gio: GIO): Unit =
    val v = value.irs
    val write = Expression.VarWrite(variable, v.result)
    gio.extend(write :: v.body)

  def call[Res: Value](func: BuildInFunction.BuildInFunction0[Res])(using gio: GIO): Res =
    val next = Expression.BuildInOperation(func, List())
    gio.add(next)
    summon[Value[Res]].indirect(next)

  def call[A: Value, Res: Value](func: BuildInFunction.BuildInFunction1[A, Res], arg: A)(using gio: GIO): Res =
    val a = arg.irs
    val next = Expression.BuildInOperation(func, List(a.result))
    gio.extend(next :: a.body)
    summon[Value[Res]].indirect(next)

  def call[A1: Value, A2: Value, Res: Value](func: BuildInFunction.BuildInFunction2[A1, A2, Res], arg1: A1, arg2: A2)(using gio: GIO): Res =
    val a1 = arg1.irs
    val a2 = arg2.irs
    val next = Expression.BuildInOperation(func, List(a1.result, a2.result))
    gio.extend(next :: a1.body ++ a2.body)
    summon[Value[Res]].indirect(next)

  def call[A1: Value, A2: Value, A3: Value, Res: Value](func: BuildInFunction.BuildInFunction3[A1, A2, A3, Res], arg1: A1, arg2: A2, arg3: A3)(using
    gio: GIO,
  ): Res =
    val a1 = arg1.irs
    val a2 = arg2.irs
    val a3 = arg3.irs
    val next = Expression.BuildInOperation(func, List(a1.result, a2.result, a3.result))
    gio.extend(next :: a1.body ++ a2.body ++ a3.body)
    summon[Value[Res]].indirect(next)

  def call[A1: Value, A2: Value, A3: Value, A4: Value, Res: Value](
    func: BuildInFunction.BuildInFunction4[A1, A2, A3, A4, Res],
    arg1: A1,
    arg2: A2,
    arg3: A3,
    arg4: A4,
  )(using gio: GIO): Res =
    val a1 = arg1.irs
    val a2 = arg2.irs
    val a3 = arg3.irs
    val a4 = arg4.irs
    val next = Expression.BuildInOperation(func, List(a1.result, a2.result, a3.result, a4.result))
    gio.extend(next :: a1.body ++ a2.body ++ a3.body ++ a4.body)
    summon[Value[Res]].indirect(next)

  def call[A: Value, Res: Value](func: CustomFunction[Res], arg: Var[A])(using gio: GIO): Res =
    val next = Expression.CustomCall(func, List(arg))
    gio.add(next)
    summon[Value[Res]].indirect(next)

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
