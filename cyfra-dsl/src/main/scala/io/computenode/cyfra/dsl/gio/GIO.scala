package io.computenode.cyfra.dsl.gio

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr
import io.computenode.cyfra.dsl.binding.{GBuffer, ReadBuffer, WriteBuffer}
import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.gio.GIO.*
import izumi.reflect.Tag

trait GIO[T]:

  def flatMap[U](f: T => GIO[U]): GIO[U] = FlatMap(this, f(this.underlying))

  def map[U](f: T => U): GIO[U] = flatMap(t => GIO.pure(f(t)))

  private[cyfra] def underlying: T

object GIO:

  case class Pure[T](value: T) extends GIO[T]:
    override def underlying: T = value

  case class FlatMap[T, U](gio: GIO[T], next: GIO[U]) extends GIO[U]:
    override def underlying: U = next.underlying

  // TODO repeat that collects results
  case class Repeat(n: Int32, f: Int32 => GIO[?]) extends GIO[Unit]:
    override def underlying: Unit = ()

  def pure[T](value: T): GIO[T] = Pure(value)

  def value[T](value: T): GIO[T] = Pure(value)

  def repeat(n: Int32)(f: Int32 => GIO[?]): GIO[Unit] =
    Repeat(n, f)

  def write[T <: Value](buffer: GBuffer[T], index: Int32, value: T): GIO[Unit] =
    WriteBuffer(buffer, index, value)

  def read[T <: Value: {FromExpr, Tag}](buffer: GBuffer[T], index: Int32): T =
    fromExpr(ReadBuffer(buffer, index))

  def invocationId: Int32 =
    fromExpr(InvocationId)
