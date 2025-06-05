package io.computenode.cyfra.vulkan.executor

import io.computenode.cyfra.vulkan.command.*
import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.vulkan.core.*
import SequenceExecutor.*
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.vulkan.memory.*
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Queue}
import io.computenode.cyfra.vulkan.compute.{ComputePipeline, InputBufferSize, LayoutSet, UniformSize}
import io.computenode.cyfra.vulkan.util.Util.*
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer, DescriptorPool, DescriptorSet}
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
private[cyfra] class SequenceExecutor(computeSequence: ComputationSequence, context: VulkanContext) {
  private val device: Device = context.device
  private val queue: Queue = context.computeQueue
  private val allocator: Allocator = context.allocator
  private val descriptorPool: DescriptorPool = context.descriptorPool
  private val commandPool: CommandPool = context.commandPool

  private val pipelineToDescriptorSets: Map[ComputePipeline, Seq[DescriptorSet]] = pushStack { stack =>
    val pipelines = computeSequence.sequence.collect { case Compute(pipeline, _) => pipeline }

    val rawSets = pipelines.map(_.computeShader.layoutInfo.sets)
    val numbered = rawSets.flatten.zipWithIndex
    val numberedSets = rawSets
      .foldLeft((numbered, Seq.empty[Seq[(LayoutSet, Int)]])) { case ((remaining, acc), sequence) =>
        val (current, rest) = remaining.splitAt(sequence.length)
        (rest, acc :+ current)
      }
      ._2

    val pipelineToIndex = pipelines.zipWithIndex.toMap
    val dependencies = computeSequence.dependencies.map { case Dependency(from, fromSet, to, toSet) =>
      (pipelineToIndex(from), fromSet, pipelineToIndex(to), toSet)
    }
    val resolvedSets = dependencies
      .foldLeft(numberedSets.map(_.toArray)) { case (sets, (from, fromSet, to, toSet)) =>
        val a = sets(from)(fromSet)
        val b = sets(to)(toSet)
        assert(a._1.bindings == b._1.bindings)
        val nextIndex = a._2 min b._2
        sets(from).update(fromSet, (a._1, nextIndex))
        sets(to).update(toSet, (b._1, nextIndex))
        sets
      }
      .map(_.toSeq.map(_._2))

    val descriptorSetMap = resolvedSets
      .zip(pipelines.map(_.descriptorSetLayouts))
      .flatMap { case (sets, layouts) =>
        sets.zip(layouts)
      }
      .distinctBy(_._1)
      .map { case (set, (id, layout)) =>
        (set, new DescriptorSet(device, id, layout.bindings, descriptorPool))
      }
      .toMap

    pipelines.zip(resolvedSets.map(_.map(descriptorSetMap(_)))).toMap
  }

  private val descriptorSets = pipelineToDescriptorSets.toSeq.flatMap(_._2).distinctBy(_.get)

  private def recordCommandBuffer(dataLength: Int): VkCommandBuffer = pushStack { stack =>
    val pipelinesHasDependencies = computeSequence.dependencies.map(_.to).toSet
    val commandBuffer = commandPool.createCommandBuffer()

    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(0)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")

    computeSequence.sequence.foreach {
      case Compute(pipeline, _) =>
        if(pipelinesHasDependencies(pipeline))
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

        val pDescriptorSets = stack.longs(pipelineToDescriptorSets(pipeline).map(_.get): _*)
        vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline.pipelineLayout, 0, pDescriptorSets, null)

        val workgroup = pipeline.computeShader.workgroupDimensions
        vkCmdDispatch(commandBuffer, dataLength / workgroup.x, 1 / workgroup.y, 1 / workgroup.z) // TODO this can be changed to indirect dispatch, this would unlock options like filters
    }

    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")
    commandBuffer
  }

  private def createBuffers(dataLength: Int): Map[DescriptorSet, Seq[Buffer]] = {

    val setToActions = computeSequence.sequence
      .collect { case Compute(pipeline, bufferActions) =>
        pipelineToDescriptorSets(pipeline).zipWithIndex.map { case (descriptorSet, i) =>
          val descriptorBufferActions = descriptorSet.bindings
            .map(_.id)
            .map(LayoutLocation(i, _))
            .map(bufferActions.getOrElse(_, BufferAction.DoNothing))
          (descriptorSet, descriptorBufferActions)
        }
      }
      .flatten
      .groupMapReduce(_._1)(_._2)((a, b) => a.zip(b).map(x => x._1 | x._2))
    

    val setToBuffers = descriptorSets.map(set =>
      val actions = setToActions(set)
      val buffers = set.bindings.zip(actions).map { case (binding, action) =>
        binding.size match
          case InputBufferSize(elemSize) =>
            new Buffer(elemSize * dataLength, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | action.action, 0, VMA_MEMORY_USAGE_GPU_ONLY, allocator)
          case UniformSize(size) =>
            new Buffer(size, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT | action.action, 0, VMA_MEMORY_USAGE_GPU_ONLY, allocator)
      }     
      set.update(buffers)
      (set, buffers)
    ).toMap

    setToBuffers
  }

  def execute(inputs: Seq[ByteBuffer], dataLength: Int): Seq[ByteBuffer] = pushStack { stack =>
    timed("Vulkan full execute"):
      val setToBuffers = createBuffers(dataLength)

      def buffersWithAction(bufferAction: BufferAction): Seq[Buffer] =
        computeSequence.sequence.collect { case x: Compute =>
          pipelineToDescriptorSets(x.pipeline).map(setToBuffers).zip(x.pumpLayoutLocations).flatMap(x => x._1.zip(x._2)).collect {
            case (buffer, action) if (action.action & bufferAction.action) != 0 => buffer
          }
        }.flatten

      val stagingBuffer = new Buffer(
        inputs.map(_.remaining()).max,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
        VMA_MEMORY_USAGE_UNKNOWN,
        allocator
      )

      buffersWithAction(BufferAction.LoadTo).zipWithIndex.foreach { case (buffer, i) =>
        Buffer.copyBuffer(inputs(i), stagingBuffer, buffer.size)
        Buffer.copyBuffer(stagingBuffer, buffer, buffer.size, commandPool).block().destroy()
      }

      val fence = new Fence(device)
      val commandBuffer = recordCommandBuffer(dataLength)
      val pCommandBuffer = stack.callocPointer(1).put(0, commandBuffer)
      val submitInfo = VkSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pCommandBuffers(pCommandBuffer)

      timed("Vulkan render command"):
        check(vkQueueSubmit(queue.get, submitInfo, fence.get), "Failed to submit command buffer to queue")
        fence.block().destroy()

      val output = buffersWithAction(BufferAction.LoadFrom).map { buffer =>
        Buffer.copyBuffer(buffer, stagingBuffer, buffer.size, commandPool).block().destroy()
        val out = BufferUtils.createByteBuffer(buffer.size)
        Buffer.copyBuffer(stagingBuffer, out, buffer.size)
        out
      }

      stagingBuffer.destroy()
      commandPool.freeCommandBuffer(commandBuffer)
      setToBuffers.keys.foreach(_.update(Seq.empty))
      setToBuffers.flatMap(_._2).foreach(_.destroy())

      output
  }

  def destroy(): Unit =
    descriptorSets.foreach(_.destroy())

}

object SequenceExecutor {
  private[cyfra] case class ComputationSequence(sequence: Seq[ComputationStep], dependencies: Seq[Dependency])

  private[cyfra] sealed trait ComputationStep
  case class Compute(pipeline: ComputePipeline, bufferActions: Map[LayoutLocation, BufferAction]) extends ComputationStep:
    def pumpLayoutLocations: Seq[Seq[BufferAction]] =
      pipeline.computeShader.layoutInfo.sets
        .map(x => x.bindings.map(y => (x.id, y.id)).map(x => bufferActions.getOrElse(LayoutLocation.apply.tupled(x), BufferAction.DoNothing)))

  case class LayoutLocation(set: Int, binding: Int)

  case class Dependency(from: ComputePipeline, fromSet: Int, to: ComputePipeline, toSet: Int)

}
