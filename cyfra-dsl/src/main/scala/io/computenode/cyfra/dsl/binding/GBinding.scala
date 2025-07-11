package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import izumi.reflect.Tag

sealed trait GBinding[T <: Value: {Tag, FromExpr}]

trait GBuffer[T <: Value: {FromExpr, Tag}] extends GBinding[T]:
  def read(index: Int32): T = FromExpr.fromExpr(ReadBuffer(this, index))

  def write(index: Int32, value: T): GIO[Unit] = GIO.write(this, index, value)

object GBuffer

trait GUniform[T <: Value: {Tag, FromExpr}] extends GBinding[T]:
  def read: T = fromExpr(ReadUniform(this))

  def write(value: T): GIO[Unit] = WriteUniform(this, value)

object GUniform:

  class ParamUniform[T <: GStruct[T]: {Tag, FromExpr}]() extends GUniform[T]

  def fromParams[T <: GStruct[T]: {Tag, FromExpr}] = ParamUniform[T]()
