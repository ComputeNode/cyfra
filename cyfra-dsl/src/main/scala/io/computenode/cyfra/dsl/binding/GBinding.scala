package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr as fromExprEval
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

sealed trait GBinding[T <: Value: {Tag, FromExpr}]:
  def tag = summon[Tag[T]]
  def fromExpr = summon[FromExpr[T]]

trait GBuffer[T <: Value: {FromExpr, Tag}] extends GBinding[T]:
  def read(index: Int32): T = FromExpr.fromExpr(ReadBuffer(this, index))

  def write(index: Int32, value: T): GIO[Empty] = GIO.write(this, index, value)

object GBuffer

trait GUniform[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}] extends GBinding[T]:
  def read: T = fromExprEval(ReadUniform(this))

  def write(value: T): GIO[Empty] = WriteUniform(this, value)
  
  def schema = summon[GStructSchema[T]]

object GUniform:

  class ParamUniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}]() extends GUniform[T]

  def fromParams[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}] = ParamUniform[T]()
