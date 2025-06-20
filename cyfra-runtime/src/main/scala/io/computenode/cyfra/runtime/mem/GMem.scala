package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.struct.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.{GContext, GFunction, UniformContext}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import izumi.reflect.Tag
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer

trait GMem[H <: Value]:
  def size: Int
  def toReadOnlyBuffer: ByteBuffer
  def map[G <: GStruct[G]: Tag: GStructSchema, R <: Value: FromExpr: Tag](
    fn: GFunction[G, H, R],
  )(using context: GContext, uc: UniformContext[G]): GMem[R] =
    context.execute(this, fn)

object GMem:
  type fRGBA = (Float, Float, Float, Float)

  def totalStride(gs: GStructSchema[?]): Int = gs.fields.map {
    case (_, fromExpr, t) if t <:< gs.gStructTag =>
      val constructor = fromExpr.asInstanceOf[GStructConstructor[?]]
      totalStride(constructor.schema)
    case (_, _, t) =>
      typeStride(t)
  }.sum

  def serializeUniform(g: GStruct[?]): ByteBuffer =
    val data = BufferUtils.createByteBuffer(totalStride(g.schema))
    g.productIterator.foreach:
      case Int32(ConstInt32(i))     => data.putInt(i)
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
    data.rewind()
    data
