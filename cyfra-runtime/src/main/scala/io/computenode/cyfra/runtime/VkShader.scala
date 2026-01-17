package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{GProgram, ExpressionProgram}
import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.binding.{GBuffer, GUniform}
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.runtime.SpirvProgram.*
import io.computenode.cyfra.vulkan.compute.ComputePipeline.*
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag

import scala.util.{Failure, Success}

case class VkShader[L](underlying: ComputePipeline, shaderBindings: L => ShaderLayout)

object VkShader:
  def apply[P, L: Layout](program: SpirvProgram[P, L])(using Device): VkShader[L] =
    val SpirvProgram(layout, dispatch, _workgroupSize, code, entryPoint, shaderBindings) = program

    val shaderLayout = shaderBindings(Layout[L].layoutRef)
    val sets = shaderLayout.map: set =>
      val descriptors = set.map:
        case Binding(binding, op) =>
          val kind = binding match
            case buffer: GBuffer[?]   => BindingType.StorageBuffer
            case uniform: GUniform[?] => BindingType.Uniform
          DescriptorInfo(kind)
      DescriptorSetInfo(descriptors)

    val pipeline = ComputePipeline(code, entryPoint, LayoutInfo(sets))
    VkShader(pipeline, shaderBindings)
