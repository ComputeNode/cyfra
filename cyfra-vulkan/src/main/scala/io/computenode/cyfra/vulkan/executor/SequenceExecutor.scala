package io.computenode.cyfra.vulkan.executor

import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.command.*
import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.vulkan.core.*
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.*
import io.computenode.cyfra.vulkan.memory.*
import io.computenode.cyfra.vulkan.util.Util.*
import org.lwjgl.BufferUtils
import org.lwjgl.util.vma.Vma.*
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.KHRSynchronization2.vkCmdPipelineBarrier2KHR
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK13.*

import java.nio.ByteBuffer

/** @author
  *   MarconZet Created 15.04.2020
  */
private[cyfra] class SequenceExecutor(computeSequence: ComputationSequence, context: VulkanContext):
  import context.given

  private val queue: Queue = context.computeQueue
  private val descriptorPool: DescriptorPool = context.descriptorPool
  private val commandPool: CommandPool = context.commandPool

  private val pipelineToDescriptorSets: Map[ComputePipeline, Seq[DescriptorSet]] = ???

  private val descriptorSets = pipelineToDescriptorSets.toSeq.flatMap(_._2).distinctBy(_.get)

  private def recordCommandBuffer() = pushStack: stack =>
    val pipelinesHasDependencies = computeSequence.dependencies.map(_.to).toSet
    val commandBuffer = commandPool.createCommandBuffer()

    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(0)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")

    computeSequence.sequence.foreach { case Compute(pipeline, _) =>
      if pipelinesHasDependencies(pipeline) then
        val memoryBarrier = VkMemoryBarrier2
          .calloc(1, stack)
          .sType$Default()
          .srcStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)
          .srcAccessMask(VK_ACCESS_2_SHADER_WRITE_BIT)
          .dstStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)
          .dstAccessMask(VK_ACCESS_2_SHADER_READ_BIT)

        val dependencyInfo = VkDependencyInfo
          .calloc(stack)
          .sType$Default()
          .pMemoryBarriers(memoryBarrier)

        vkCmdPipelineBarrier2KHR(commandBuffer, dependencyInfo)

      vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline.get)

      val pDescriptorSets = stack.longs(pipelineToDescriptorSets(pipeline).map(_.get)*)
      vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline.pipelineLayout.id, 0, pDescriptorSets, null)

      vkCmdDispatch(commandBuffer, 8, 1, 1)
    }

    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")
    commandBuffer

  private def createBuffers(): Map[DescriptorSet, Seq[Buffer]] =
    val setToActions = ???
    val setToBuffers = ???
    setToBuffers

  def execute(inputs: Seq[ByteBuffer]): Seq[ByteBuffer] = pushStack: stack =>
    timed("Vulkan full execute"):
      val setToBuffers = createBuffers()

      def buffersWithAction(bufferAction: BufferAction): Seq[Buffer] =
        computeSequence.sequence.collect { case x: Compute =>
          pipelineToDescriptorSets(x.pipeline)
            .map(setToBuffers)
            .zip(x.pumpLayoutLocations)
            .flatMap(x => x._1.zip(x._2))
            .collect:
              case (buffer, action) if (action.action & bufferAction.action) != 0 => buffer
        }.flatten

      val stagingBuffer =
        new Buffer.HostBuffer(inputs.map(_.remaining()).max, VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT)

      buffersWithAction(BufferAction.LoadTo).zipWithIndex.foreach { case (buffer, i) =>
        Buffer.copyBuffer(inputs(i), stagingBuffer, buffer.size, 0, 0)
        Buffer.copyBuffer(stagingBuffer, buffer, buffer.size, 0, 0, commandPool).block().destroy()
      }

      val fence = new Fence()
      val commandBuffer = recordCommandBuffer()
      val pCommandBuffer = stack.callocPointer(1).put(0, commandBuffer)
      val submitInfo = VkSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pCommandBuffers(pCommandBuffer)

      timed("Vulkan render command"):
        check(vkQueueSubmit(queue.get, submitInfo, fence.get), "Failed to submit command buffer to queue")
        fence.block().destroy()

      val output = buffersWithAction(BufferAction.LoadFrom).map { buffer =>
        Buffer.copyBuffer(buffer, stagingBuffer, 0, 0, buffer.size, commandPool).block().destroy()
        val out = BufferUtils.createByteBuffer(buffer.size)
        Buffer.copyBuffer(stagingBuffer, out, 0, 0, buffer.size)
        out
      }

      stagingBuffer.destroy()
      commandPool.freeCommandBuffer(commandBuffer)
      setToBuffers.flatMap(_._2).foreach(_.destroy())

      output

  def destroy(): Unit = ???

object SequenceExecutor:
  private[cyfra] case class ComputationSequence(sequence: Seq[ComputationStep], dependencies: Seq[Dependency])

  private[cyfra] sealed trait ComputationStep
  case class Compute(pipeline: ComputePipeline, bufferActions: Map[LayoutLocation, BufferAction]) extends ComputationStep:
    def pumpLayoutLocations: Seq[Seq[BufferAction]] = ???

  case class LayoutLocation(set: Int, binding: Int)

  case class Dependency(from: ComputePipeline, fromSet: Int, to: ComputePipeline, toSet: Int)

  enum BufferAction(val action: Int):
    case DoNothing extends BufferAction(0)
    case LoadTo extends BufferAction(VK_BUFFER_USAGE_TRANSFER_DST_BIT)
    case LoadFrom extends BufferAction(VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
    case LoadFromTo extends BufferAction(VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT)

    private def findAction(action: Int): BufferAction = action match
      case VK_BUFFER_USAGE_TRANSFER_DST_BIT => LoadTo
      case VK_BUFFER_USAGE_TRANSFER_SRC_BIT => LoadFrom
      case 3                                => LoadFromTo
      case _                                => DoNothing

    def |(other: BufferAction): BufferAction = findAction(this.action | other.action)
