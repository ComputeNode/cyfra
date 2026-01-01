package io.computenode.cyfra.dsl.monad

import io.computenode.cyfra.core.expression.{Bool, BuildInFunction, CustomFunction, JumpTarget, UInt32, Value, Var, given}
import io.computenode.cyfra.core.binding.GBuffer
import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.utility.cats.Free

sealed trait GOps[T: Value]:
  def v: Value[T] = Value[T]

object GOps:
  case class ReadBuffer[T: Value](buffer: GBuffer[T], index: UInt32) extends GOps[T]
  case class WriteBuffer[T: Value](buffer: GBuffer[T], index: UInt32, value: T) extends GOps[Unit]:
    def tv: Value[T] = Value[T]
  case class DeclareVariable[T: Value](variable: Var[T]) extends GOps[Unit]:
    def tv: Value[T] = Value[T]
  case class ReadVariable[T: Value](variable: Var[T]) extends GOps[T]
  case class WriteVariable[T: Value](variable: Var[T], value: T) extends GOps[Unit]:
    def tv: Value[T] = Value[T]
  case class CallBuildIn0[Res: Value](func: BuildInFunction.BuildInFunction0[Res]) extends GOps[Res]
  case class CallBuildIn1[A: Value, Res: Value](func: BuildInFunction.BuildInFunction1[A, Res], arg: A) extends GOps[Res]:
    def tv: Value[A] = summon[Value[A]]
  case class CallBuildIn2[A1: Value, A2: Value, Res: Value](func: BuildInFunction.BuildInFunction2[A1, A2, Res], arg1: A1, arg2: A2)
      extends GOps[Res]:
    def tv1: Value[A1] = summon[Value[A1]]
    def tv2: Value[A2] = summon[Value[A2]]
  case class CallBuildIn3[A1: Value, A2: Value, A3: Value, Res: Value](
    func: BuildInFunction.BuildInFunction3[A1, A2, A3, Res],
    arg1: A1,
    arg2: A2,
    arg3: A3,
  ) extends GOps[Res]:
    def tv1: Value[A1] = summon[Value[A1]]
    def tv2: Value[A2] = summon[Value[A2]]
    def tv3: Value[A3] = summon[Value[A3]]
  case class CallBuildIn4[A1: Value, A2: Value, A3: Value, A4: Value, Res: Value](
    func: BuildInFunction.BuildInFunction4[A1, A2, A3, A4, Res],
    arg1: A1,
    arg2: A2,
    arg3: A3,
    arg4: A4,
  ) extends GOps[Res]:
    def tv1: Value[A1] = summon[Value[A1]]
    def tv2: Value[A2] = summon[Value[A2]]
    def tv3: Value[A3] = summon[Value[A3]]
    def tv4: Value[A4] = summon[Value[A4]]
  case class CallCustom1[A: Value, Res: Value](func: CustomFunction[Res], arg: Var[A]) extends GOps[Res]:
    def tv: Value[A] = summon[Value[A]]
  case class Branch[T: Value](cond: Bool, ifTrue: GIO[T], ifFalse: GIO[T], break: JumpTarget[T]) extends GOps[T]
  case class Loop(mainBody: GIO[Unit], continueBody: GIO[Unit], break: BreakTarget, continue: ContinueTarget) extends GOps[Unit]
  case class ConditionalJump[T: Value](cond: Bool, target: JumpTarget[T], value: T) extends GOps[Unit]:
    def tv: Value[T] = Value[T]
  case class Jump[T: Value](target: JumpTarget[T], value: T) extends GOps[Unit]:
    def tv: Value[T] = Value[T]

  def read[T: Value](buffer: GBuffer[T], index: UInt32): GIO[T] =
    Free.liftF[GOps, T](ReadBuffer(buffer, index))

  def write[T: Value](buffer: GBuffer[T], index: UInt32, value: T): GIO[Unit] =
    Free.liftF[GOps, Unit](WriteBuffer(buffer, index, value))

  def declare[T: Value]: GIO[Var[T]] =
    val variable = Var[T]()
    Free.liftF[GOps, Unit](DeclareVariable(variable)).map(_ => variable)

  def read[T: Value](variable: Var[T]): GIO[T] =
    Free.liftF[GOps, T](ReadVariable(variable))

  def write[T: Value](variable: Var[T], value: T): GIO[Unit] =
    Free.liftF[GOps, Unit](WriteVariable(variable, value))

  def call[Res: Value](func: BuildInFunction.BuildInFunction0[Res]): GIO[Res] =
    Free.liftF[GOps, Res](CallBuildIn0(func))

  def call[A: Value, Res: Value](func: BuildInFunction.BuildInFunction1[A, Res], arg: A): GIO[Res] =
    Free.liftF[GOps, Res](CallBuildIn1(func, arg))

  def call[A1: Value, A2: Value, Res: Value](func: BuildInFunction.BuildInFunction2[A1, A2, Res], arg1: A1, arg2: A2): GIO[Res] =
    Free.liftF[GOps, Res](CallBuildIn2(func, arg1, arg2))

  def call[A1: Value, A2: Value, A3: Value, Res: Value](
    func: BuildInFunction.BuildInFunction3[A1, A2, A3, Res],
    arg1: A1,
    arg2: A2,
    arg3: A3,
  ): GIO[Res] =
    Free.liftF[GOps, Res](CallBuildIn3(func, arg1, arg2, arg3))

  def call[A1: Value, A2: Value, A3: Value, A4: Value, Res: Value](
    func: BuildInFunction.BuildInFunction4[A1, A2, A3, A4, Res],
    arg1: A1,
    arg2: A2,
    arg3: A3,
    arg4: A4,
  ): GIO[Res] =
    Free.liftF[GOps, Res](CallBuildIn4(func, arg1, arg2, arg3, arg4))

  def call[A: Value, Res: Value](func: CustomFunction[Res], arg: Var[A]): GIO[Res] =
    Free.liftF[GOps, Res](CallCustom1(func, arg))

  def branch[T: Value](cond: Bool)(ifTrue: JumpTarget[T] => GIO[T])(ifFalse: JumpTarget[T] => GIO[T]): GIO[T] =
    val target = JumpTarget()
    Free.liftF[GOps, T](Branch(cond, ifTrue(target), ifFalse(target), target))

  def loop(body: (BreakTarget, ContinueTarget) ?=> GIO[Unit], continue: GIO[Unit]): GIO[Unit] =
    val (b, c) = (BreakTarget(), ContinueTarget())
    Free.liftF[GOps, Unit](Loop(body(using b, c), continue, b, c))

  def jump[T: Value](target: JumpTarget[T], value: T): GIO[Unit] =
    Free.liftF[GOps, Unit](Jump(target, value))

  def conditionalJump[T: Value](cond: Bool, target: JumpTarget[T], value: T): GIO[Unit] =
    Free.liftF[GOps, Unit](ConditionalJump(cond, target, value))

  def pure[T: Value](value: T): GIO[T] =
    Free.pure[GOps, T](value)
