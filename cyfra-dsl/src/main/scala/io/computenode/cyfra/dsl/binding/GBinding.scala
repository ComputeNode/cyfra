package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr as fromExprEval
import io.computenode.cyfra.dsl.Value.{FloatType, FromExpr, Int32, Vec4}
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

object GBuffer:
  /** Extension to read 4 consecutive elements as Vec4 from a float buffer.
    * @param buffer The buffer to read from
    * @param index Base index - reads elements at index, index+1, index+2, index+3
    * @return Vec4 containing the 4 consecutive values
    */
  extension [T <: FloatType: Tag: FromExpr](buffer: GBuffer[T])
    def readVec4(index: Int32)(using Tag[Vec4[T]]): Vec4[T] = 
      FromExpr.fromExpr[Vec4[T]](ReadBufferVec4(buffer, index))

trait GUniform[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}] extends GBinding[T]:
  def read: T = fromExprEval(ReadUniform(this))

  def write(value: T): GIO[Empty] = WriteUniform(this, value)

  def schema = summon[GStructSchema[T]]

object GUniform:

  class ParamUniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}]() extends GUniform[T]

  def fromParams[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}] = ParamUniform[T]()
