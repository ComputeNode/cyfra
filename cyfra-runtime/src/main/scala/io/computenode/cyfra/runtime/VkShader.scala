package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{GProgram, GioProgram, SpirvProgram}
import io.computenode.cyfra.core.SpirvProgram.*
import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.compute.ComputePipeline.*
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag

import scala.util.{Failure, Success}

case class VkShader[L](underlying: ComputePipeline, shaderBindings: L => ShaderLayout):
  def name: String = underlying.name

object VkShader:
  def apply[P, L: Layout](program: SpirvProgram[P, L], name: String = "Shader")(using Device): VkShader[L] =
    val SpirvProgram(layout, dispatch, workgroupSize, code, entryPoint, shaderBindings, _) = program

    val shaderLayout = shaderBindings(Layout[L].layoutRef)
    val sets = shaderLayout.map: set =>
      val descriptors = set.map:
        case Binding(binding, op) =>
          val kind = binding match
            case buffer: GBuffer[?]   => BindingType.StorageBuffer
            case uniform: GUniform[?] => BindingType.Uniform
          DescriptorInfo(kind)
      DescriptorSetInfo(descriptors)

    val pipeline = ComputePipeline(code, entryPoint, LayoutInfo(sets), name)
    VkShader(pipeline, shaderBindings)
