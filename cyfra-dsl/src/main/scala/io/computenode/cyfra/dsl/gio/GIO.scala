package io.computenode.cyfra.dsl.gio

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr
import io.computenode.cyfra.dsl.buffer.{GBuffer, Read, Write}
import io.computenode.cyfra.dsl.gio.GIO.*

trait GIO[T]:

  def flatMap[U](f: T => GIO[U]): GIO[U] = FlatMap(this, f(this.underlying))

  def map[U](f: T => U): GIO[U] = flatMap(t => GIO.pure(f(t)))

  private[cyfra] def underlying: T

object GIO:

  case class Pure[T](value: T) extends GIO[T]:
    override def underlying: T = value

  case class FlatMap[T, U](gio: GIO[T], next: GIO[U]) extends GIO[U]:
    override def underlying: U = next.underlying
  

  def pure[T](value: T): GIO[T] = Pure(value)

  def value[T](value: T): GIO[T] = Pure(value)
  
  def write[T <: Value](buffer: GBuffer[T], index: Int, value: T): GIO[Unit] =
    Write(buffer, index, value)
    
  def read[T <: Value : FromExpr](buffer: GBuffer[T], index: Int): T =
    fromExpr(Read(buffer, index))

