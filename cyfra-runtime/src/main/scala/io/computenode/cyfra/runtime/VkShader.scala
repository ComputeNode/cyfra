package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.SpirvProgram
import io.computenode.cyfra.core.SpirvProgram.*
import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.GioProgram
import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.compute.ComputePipeline.*
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag

case class VkShader[L](underlying: ComputePipeline, shaderBindings: L => ShaderLayout)

object VkShader:
  def apply[P, L <: Layout: LayoutStruct](program: GProgram[P, L])(using Device): VkShader[L] =
    val SpirvProgram(layout, dispatch, _workgroupSize, code, entryPoint, shaderBindings, _) = program match
      case p: GioProgram[?, ?]   => compile(p)
      case p: SpirvProgram[?, ?] => p
      case _                     => throw new IllegalArgumentException(s"Unsupported program type: ${program.getClass.getName}")

    val shaderLayout = shaderBindings(summon[LayoutStruct[L]].layoutRef)
    val sets = shaderLayout.map: set =>
      val descriptors = set.map { case Binding(binding, op) =>
        val kind = binding match
          case buffer: GBuffer[?]   => BindingType.StorageBuffer
          case uniform: GUniform[?] => BindingType.Uniform
        DescriptorInfo(kind)
      }
      DescriptorSetInfo(descriptors)

    val pipeline = ComputePipeline(code, entryPoint, LayoutInfo(sets))
    VkShader(pipeline, shaderBindings)

  def compile[Params, L <: Layout: LayoutStruct](program: GioProgram[Params, L]): SpirvProgram[Params, L] = ???
