package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.{GStruct, GStructConstructor, GStructSchema, Value}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Expression.*
import GStruct.Empty
import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.runtime.{GFunction, GContext}

import izumi.reflect.Tag
import java.nio.ByteBuffer
import io.computenode.cyfra.vulkan.memory.Buffer 

trait GMem[H <: Value : Tag : FromExpr]: 
  def size: Int
  def vulkanBuffer: Buffer

  def map[
    G <: GStruct[G] : Tag : GStructSchema,
    R <: Value : FromExpr : Tag
  ](uniformStruct: G, fn: GFunction[G, H, R])(using context: GContext): GMem[R] =
    context.execute(this, uniformStruct, fn)

  def map[R <: Value : FromExpr : Tag]
    (fn: GFunction[GStruct.Empty, H, R])(using context: GContext): GMem[R] =
    context.execute(this, fn) 

  def cleanup(): Unit
end GMem

object GMem:
  type fRGBA = (Float, Float, Float, Float)

  def totalStride(gs: GStructSchema[_]): Int = gs.fields.map {
    case (_, fromExpr, t) if t <:< gs.gStructTag =>
      val constructor = fromExpr.asInstanceOf[GStructConstructor[_]]
      totalStride(constructor.schema)
    case (_, _, t) =>
      typeStride(t)
  }.sum

  def serializeUniform(g: GStruct[?]): ByteBuffer = {
    val data = ByteBuffer.allocateDirect(totalStride(g.schema))
    g.productIterator.foreach {
      case Int32(ConstInt32(i)) => data.putInt(i)
      case Float32(ConstFloat32(f)) => data.putFloat(f)
      case Vec4(ComposeVec4(Float32(ConstFloat32(x)), Float32(ConstFloat32(y)), Float32(ConstFloat32(z)), Float32(ConstFloat32(a)))) =>
        data.putFloat(x)
        data.putFloat(y)
        data.putFloat(z)
        data.putFloat(a)
      case Vec3(ComposeVec3(Float32(ConstFloat32(x)), Float32(ConstFloat32(y)), Float32(ConstFloat32(z)))) =>
        data.putFloat(x)
        data.putFloat(y)
        data.putFloat(z)
      case Vec2(ComposeVec2(Float32(ConstFloat32(x)), Float32(ConstFloat32(y)))) =>
        data.putFloat(x)
        data.putFloat(y)
      case illegal =>
        throw new IllegalArgumentException(s"Uniform must be constructed from constants (got field $illegal)")
    }
    data.rewind()
    data
  }
