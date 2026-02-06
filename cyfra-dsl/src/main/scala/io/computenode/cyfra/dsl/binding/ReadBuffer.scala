package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value.{Float16, Float32, Int32, Vec4}
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

case class ReadBuffer[T <: Value: Tag](buffer: GBuffer[T], index: Int32) extends Expression[T]

/** Reads 4 consecutive elements from a buffer and returns them as a Vec4.
  * The index is the base index - elements at index, index+1, index+2, index+3 are read.
  * This compiles to 4 scalar loads + OpCompositeConstruct.
  * 
  * Note: For truly coalesced Vec4 loads, use GBufferVec4 which declares the buffer
  * with Vec4 element type.
  */
case class ReadBufferVec4[T <: Value.FloatType: Tag](buffer: GBuffer[T], index: Int32)(using vecTag: Tag[Vec4[T]]) 
  extends Expression[Vec4[T]](using vecTag)
