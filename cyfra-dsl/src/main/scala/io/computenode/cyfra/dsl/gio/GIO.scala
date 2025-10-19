package io.computenode.cyfra.dsl.gio

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr
import io.computenode.cyfra.dsl.binding.{GBuffer, ReadBuffer, WriteBuffer}
import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.gio.GIO.*
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.dsl.control.When
import izumi.reflect.Tag

trait GIO[T <: Value]:

  def flatMap[U <: Value](f: T => GIO[U]): GIO[U] = FlatMap(this, f(this.underlying))

  def map[U <: Value](f: T => U): GIO[U] = flatMap(t => GIO.pure(f(t)))

  private[cyfra] def underlying: T

object GIO:

  case class Pure[T <: Value](value: T) extends GIO[T]:
    override def underlying: T = value

  case class FlatMap[T <: Value, U <: Value](gio: GIO[T], next: GIO[U]) extends GIO[U]:
    override def underlying: U = next.underlying

  // TODO repeat that collects results
  case class Repeat(n: Int32, f: GIO[?]) extends GIO[Empty]:
    override def underlying: Empty = Empty()

  case class Printf(format: String, args: Value*) extends GIO[Empty]:
    override def underlying: Empty = Empty()

  def pure[T <: Value](value: T): GIO[T] = Pure(value)

  def value[T <: Value](value: T): GIO[T] = Pure(value)

  case object CurrentRepeatIndex extends PhantomExpression[Int32] with CustomTreeId:
    override val treeid: Int = treeidState.getAndIncrement()

  def repeat(n: Int32)(f: Int32 => GIO[?]): GIO[Empty] =
    Repeat(n, f(fromExpr(CurrentRepeatIndex)))

  def write[T <: Value](buffer: GBuffer[T], index: Int32, value: T): GIO[Empty] =
    WriteBuffer(buffer, index, value)

  def printf(format: String, args: Value*): GIO[Empty] =
    Printf(s"|$format", args*)

  def when(cond: GBoolean)(thenCode: GIO[?]): GIO[Empty] =
    val n = When.when(cond)(1: Int32).otherwise(0)
    repeat(n): _ =>
      thenCode

  def read[T <: Value: {FromExpr, Tag}](buffer: GBuffer[T], index: Int32): T =
    fromExpr(ReadBuffer(buffer, index))

  def invocationId: Int32 =
    fromExpr(InvocationId)
