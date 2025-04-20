package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.*
import io.computenode.cyfra.vulkan.executor.{BufferAction, SequenceExecutor}
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.runtime.mem.GMem.totalStride
import io.computenode.cyfra.runtime.{GArray2DFunction, GContext, GFunction}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.utility.Utility.timed
import izumi.reflect.Tag
import org.lwjgl.system.MemoryUtil

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext.Implicits.*
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

trait WritableGMem[T <: Value, R] extends GMem[T]:
  def stride: Int

  val data = MemoryUtil.memAlloc(size * stride)

  protected def toResultArray(buffer: ByteBuffer): Array[R]

  def map(fn: GFunction[T, T])(implicit context: GContext): Future[Array[R]] =
    execute(fn.pipeline)(using context, UniformContext.empty)

  def map[G <: GStruct[G] : Tag : GStructSchema](fn: GArray2DFunction[G, T, T])(implicit context: GContext, uniformContext: UniformContext[G]): Future[Array[R]] =
    execute(fn.pipeline)

  private def execute(pipeline: ComputePipeline)(implicit context: GContext, uniformContext: UniformContext[_]) = {
    val isUniformEmpty = uniformContext.uniform.schema.fields.isEmpty
    val actions = Map(
      LayoutLocation(0, 0) -> BufferAction.LoadTo,
      LayoutLocation(0, 1) -> BufferAction.LoadFrom
    ) ++ (if (isUniformEmpty) Map.empty else Map(LayoutLocation(0, 2) -> BufferAction.LoadTo))
    val sequence = ComputationSequence(Seq(Compute(pipeline, actions)), Seq.empty)
    val executor = new SequenceExecutor(sequence, context.vkContext)
    val inData = if (isUniformEmpty) Seq(data) else Seq(data, serializeUniform(uniformContext.uniform))
    Future {
      val out = executor.execute(inData, size)
      executor.destroy()
      toResultArray(out.head)
    }
  }

  private def serializeUniform(g: GStruct[_]): ByteBuffer = {

    val data = MemoryUtil.memAlloc(totalStride(g.schema))

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
