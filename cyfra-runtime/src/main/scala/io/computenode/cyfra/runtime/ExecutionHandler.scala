package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.SpirvProgram.ShaderLayout
import io.computenode.cyfra.core.{GExecution, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.ExecutionHandler.ShaderCall
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.core.Queue
import io.computenode.cyfra.vulkan.memory.DescriptorPool
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkCommandBufferBeginInfo

class ExecutionHandler(runtime: VkCyfraRuntime):
  private val context = runtime.context
  import context.given

  private val queue: Queue = context.computeQueue
  private val descriptorPool: DescriptorPool = context.descriptorPool
  private val commandPool: CommandPool = context.commandPool

  def handle[Params, L <: Layout, RL <: Layout](execution: GExecution[Params, L, RL], params: Params, layout: L): RL = pushStack: stack =>
    val (result, calls) = interpret(execution, params, layout)

    ???

    val commandBuffer = commandPool.createCommandBuffer()
    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(0)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")
    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")
    ???

  private def interpret[Params, L <: Layout, RL <: Layout](execution: GExecution[Params, L, RL], params: Params, layout: L): (RL, Seq[ShaderCall]) =
    execution match
      case GExecution.Pure()                                                          => (layout, Seq.empty)
      case GExecution.Map(nextExecution, mapResult, contramapLayout, contramapParams) =>
        val cParams = contramapParams(params)
        val cLayout = contramapLayout(layout)
        val (prevLayout, calls) = interpret(nextExecution, cParams, cLayout)
        (mapResult(prevLayout), calls)
      case GExecution.FlatMap(execution, f) =>
        val (prevLayout, calls) = interpret(execution, params, layout)
        val nextExecution = f(params, prevLayout)
        val (prevLayout2, calls2) = interpret(nextExecution, params, layout)
        (prevLayout2, calls ++ calls2)
      case program: GProgram[?, ?] =>
//        val shader = runtime.getOrLoadProgram(program)
        val shader = runtime.shaderCache(program.cacheKey).asInstanceOf[VkShader[L]]
        val rl = layout.asInstanceOf[RL]
        (rl, Seq(ShaderCall(shader.underlying, shader.shaderBindings(layout))))
      case _ => ???

object ExecutionHandler:
  case class ShaderCall(pipeline: ComputePipeline, layout: ShaderLayout)
