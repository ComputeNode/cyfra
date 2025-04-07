package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.*
import io.computenode.cyfra.vulkan.executor.{BufferAction, SequenceExecutor}
import io.computenode.cyfra.*
import io.computenode.cyfra.utility.Utility.timed
import izumi.reflect.Tag
import org.lwjgl.system.MemoryUtil

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext.Implicits.*
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

trait GMem[H <: Value : Tag : FromExpr]:
  def size: Int
  val data: ByteBuffer

  // construct the GFunction and call execute on it.
  def map[R <: Value: Tag : FromExpr](fn: H => R)(using context: GContext): Future[Array[R]] =
    val gFun : GFunction[Empty, H, R] = GFunction(
      size, // ??? = width
      size, // ??? = height
      (e: Empty, nums: (Int32, Int32), arr: GArray2D[H]) => fn(arr.at(nums._1, nums._2))
    )
    context.execute(this, gFun)

trait WritableGMem[T <: Value, R] extends GMem[T]:
  def stride: Int
  val data = MemoryUtil.memAlloc(size * stride)

  protected def toResultArray(buffer: ByteBuffer): Array[R]

  // stub for map, for now the old one is used to make things compile.
  // def map[R](fn: T => T): Future[Array[R]] = ???

  def map[G <: GStruct[G] : Tag: GStructSchema](fn: GFunction[G, T, T])(implicit context: GContext, uniformContext: UniformContext[G]): Future[Array[R]] =
    execute(fn.pipeline)

  private def execute(pipeline: ComputePipeline)(implicit context: GContext, uniformContext: UniformContext[_]) = {
    val isUniformEmpty = uniformContext.uniform.schema.fields.isEmpty
    val actions = Map(
      LayoutLocation(0, 0) -> BufferAction.LoadTo,
      LayoutLocation(0, 1) -> BufferAction.LoadFrom
    ) ++ (if(isUniformEmpty) Map.empty else Map(LayoutLocation(0, 2) -> BufferAction.LoadTo))
    val sequence = ComputationSequence(Seq(Compute(pipeline, actions)), Seq.empty)
    val executor = new SequenceExecutor(sequence, context.vkContext)
    val inData = if(isUniformEmpty) Seq(data) else Seq(data, serializeUniform(uniformContext.uniform))
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
  // stub for map
  override def map[R](fn: Float32 => Float32)(using uniformContext: UniformContext[_]): Future[Array[R]] = ???
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
  // stub for map
  override def map[R](fn: Vec4[Float32] => Vec4[Float32])(using uniformContext: UniformContext[_]): Future[Array[R]] = ???

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
