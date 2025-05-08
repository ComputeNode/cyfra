package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.dsl.{GStruct, GStructSchema, Value, UniformContext}
import GStruct.Empty, Value.{Float32, Vec4}
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.executor.{BufferAction, SequenceExecutor}
import SequenceExecutor.*
import mem.{GMem, FloatMem, Vec4FloatMem}
import org.lwjgl.system.MemoryUtil
import izumi.reflect.Tag


trait GContext {
  val vkContext = new VulkanContext(enableValidationLayers = true)
  def compile[G <: GStruct[G] : Tag: GStructSchema, H <: Value: Tag: FromExpr, R <: Value: Tag: FromExpr](function: GFunction[G, H, R]): ComputePipeline

  def execute[
    G <: GStruct[G] : Tag : GStructSchema,
    H <: Value,
    R <: Value
  ](mem: GMem[H], fn: GFunction[?, H, R])(using uniformContext: UniformContext[_]): GMem[R] =
    val isUniformEmpty = uniformContext.uniform.schema.fields.isEmpty
    val actions = Map(
      LayoutLocation(0, 0) -> BufferAction.LoadTo,
      LayoutLocation(0, 1) -> BufferAction.LoadFrom
    ) ++ (
      if isUniformEmpty then Map.empty 
      else Map(LayoutLocation(0, 2) -> BufferAction.LoadTo)
    )
    val sequence = ComputationSequence(Seq(Compute(fn.pipeline, actions)), Seq.empty)
    val executor = new SequenceExecutor(sequence, vkContext)
    
    val data = mem.toReadOnlyBuffer
    val inData =
      if isUniformEmpty then Seq(data)
      else Seq(data, GMem.serializeUniform(uniformContext.uniform))
    val out = executor.execute(inData, mem.size)
    executor.destroy()

    val inTags = fn.arrayInputs
    assert(inTags.size == 1)

    inTags.head match
      case t if t == Tag[Float32] =>
        new FloatMem(mem.size, out.head).asInstanceOf[GMem[R]]
      case t if t == Tag[Vec4[Float32]] =>
        new Vec4FloatMem(mem.size, out.head).asInstanceOf[GMem[R]]
}
