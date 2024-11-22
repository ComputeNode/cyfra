package com.scalag

import com.scalag.Expression.*
import com.scalag.Value
import com.scalag.Value.*
import com.scalag.vulkan.compute.ComputePipeline
import com.scalag.vulkan.executor.SequenceExecutor.*
import com.scalag.vulkan.executor.{BufferAction, SequenceExecutor}
import izumi.reflect.Tag
import org.lwjgl.system.MemoryUtil

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext.Implicits.*
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

trait GMem[H <: Value]:
  def size: Int
  val data: ByteBuffer

trait WritableGMem[T <: Value, R] extends GMem[T]:
  def stride: Int
  val data = MemoryUtil.memAlloc(size * stride)
  
  protected def toResultArray(buffer: ByteBuffer): Array[R]

  def map(fn: GFunction[T, T])(implicit context: GContext): Future[Array[R]] =
    execute(fn.pipeline)(using context, UniformContext.empty)

  def map[G <: GStruct[G] : Tag: GStructSchema](fn: GArray2DFunction[G, T, T])(implicit context: GContext, uniformContext: UniformContext[G]): Future[Array[R]] =
    execute(fn.pipeline)

  private def execute(pipeline: ComputePipeline)(implicit context: GContext, uniformContext: UniformContext[_]) = {
    val actions = Map(
      LayoutLocation(0, 0) -> BufferAction.LoadTo, 
      LayoutLocation(0, 1) -> BufferAction.LoadFrom,
      LayoutLocation(0, 2) -> BufferAction.LoadTo
    )
    val sequence = ComputationSequence(Seq(Compute(pipeline, actions)), Seq.empty)
    val executor = new SequenceExecutor(sequence, context.vkContext)
    val inData = if(uniformContext.uniform.schema.fields.isEmpty) 
      Seq(data) 
    else 
      Seq(data, serializeUniform(uniformContext.uniform))
    Future {
      val out = executor.execute(inData, size)
      executor.destroy()
      toResultArray(out.head)
    }
  }
  
  private def serializeUniform(g: GStruct[_]): ByteBuffer = {
    val data = MemoryUtil.memAlloc(g.schema.totalStride)
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

  def write(data: Array[R]): Unit

class FloatMem(val size: Int) extends WritableGMem[Float32, Float]:

  def stride: Int = 4

  override protected def toResultArray(buffer: ByteBuffer): Array[Float] = {
    val res = buffer.asFloatBuffer()
    val result = new Array[Float](size)
    res.get(result)
    result
  }

  def write(floats: Array[Float]): Unit = {
    data.rewind()
    data.asFloatBuffer().put(floats)
    data.rewind()
  }

object FloatMem {
  def apply(floats: Array[Float]): FloatMem = {
    val floatMem = new FloatMem(floats.length)
    floatMem.write(floats)
    floatMem
  }

  def apply(size: Int): FloatMem =
    new FloatMem(size)
}

type RGBA = (Float, Float, Float, Float)
class Vec4FloatMem(val size: Int) extends WritableGMem[Vec4[Float32], RGBA]:
  def stride: Int = 16

  override protected def toResultArray(buffer: ByteBuffer): Array[RGBA] = {
    val res = buffer.asFloatBuffer()
    val result = new Array[RGBA](size)
    for (i <- 0 until size)
      result(i) = (res.get(), res.get(), res.get(), res.get())
    result
  }

  def write(vecs: Array[RGBA]): Unit = {
    data.rewind()
    vecs.foreach { case (x, y, z, a) =>
      data.putFloat(x)
      data.putFloat(y)
      data.putFloat(z)
      data.putFloat(a)
    }
    data.rewind()
  }

object Vec4FloatMem:
  def apply(vecs: Array[RGBA]): Vec4FloatMem = {
    val vec4FloatMem = new Vec4FloatMem(vecs.length)
    vec4FloatMem.write(vecs)
    vec4FloatMem
  }

  def apply(size: Int): Vec4FloatMem =
    new Vec4FloatMem(size)
