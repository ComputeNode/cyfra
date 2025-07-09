package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{GExecution, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.vulkan.command.CommandPool
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
    val commandBuffer = commandPool.createCommandBuffer()

    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(0)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")
    record(execution, params, layout)
    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")

    ???

  private def record[Params, L <: Layout, RL <: Layout](execution: GExecution[Params, L, RL], params: Params, layout: L): RL =
    execution match
      case GExecution.Pure()                                                      => ???
      case GExecution.Map(nextExecution, mapResult, contramapLayout, contramapParams) =>
        val cParams = contramapParams(params)
        val cLayout = contramapLayout(layout)
        record(nextExecution, cParams, cLayout)
      case GExecution.FlatMap(execution, f) => ???
      case program: GProgram[?, ?]          => ???
      case _                                => ???

object ExecutionHandler
