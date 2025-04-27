package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.dsl.{GStruct, GStructSchema, Value}
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import izumi.reflect.Tag

import scala.concurrent.Future

trait GContext {
  val vkContext = new VulkanContext(enableValidationLayers = true)

  def compile[
    G <: GStruct[G] : GStructSchema : Tag, 
    H <: Value: Tag: FromExpr, 
    R <: Value: Tag
  ](function: GFunction[G, H, R]): ComputePipeline
  def compile[G <: GStruct[G] : Tag: GStructSchema, H <: Value: Tag: FromExpr, R <: Value: Tag: FromExpr](function: GArray2DFunction[G, H, R]): ComputePipeline
}