package io.computenode.cyfra.api

import io.computenode.cyfra.utility.Color.*
import io.computenode.cyfra.dsl.MVPContext
import io.computenode.cyfra.vulkan.memory.Buffer
import io.computenode.cyfra.vulkan.{VulkanContext}
import io.computenode.cyfra.vulkan.util.Util.*
import io.computenode.cyfra.vulkan.executor.SequenceExecutor
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.{ComputationSequence, Compute, Dependency}
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import java.nio.ByteBuffer
import scala.util.Using

/**
  * Manages underlying Vulkan resources and provides a user-friendly API
  * for creating pipelines, executing computations, and handling buffers.
  *
  * @param enableValidation enable Vulkan validation layers if supported
  */
class ComputeContext(enableValidation: Boolean = false) extends AutoCloseable {

  // Internal Vulkan context
  private val vkContext = new VulkanContext(enableValidation)
  private val gContext = new MVPContext() // internally uses a VulkanContext too

  /** Simplified method to run a single compute pipeline with input buffers. */
  def runComputePipeline(pipeline: ComputePipeline, inputs: Seq[Buffer], dataLength: Int): Seq[Buffer] = {
    val sequence = ComputationSequence(
      Seq(Compute(pipeline, Map.empty)),
      Seq.empty[Dependency]
    )
    val executor = new SequenceExecutor(sequence, vkContext)
    executor.execute(inputs.map(_.mapToByteBuffer()), dataLength).map(Buffer.wrapByteBuffer(_, vkContext.allocator))
  }

  /** Allocates GPU memory. */
  def allocateBuffer(size: Int, usage: Int): Buffer =
    new Buffer(size, usage, 0, org.lwjgl.util.vma.Vma.VMA_MEMORY_USAGE_GPU_ONLY, vkContext.allocator)

  /** Frees a previously allocated buffer. */
  def freeBuffer(buffer: Buffer): Unit =
    buffer.destroy()

  /** Transfers data from CPU to GPU. */
  def uploadData(buffer: Buffer, data: Array[Byte]): Unit = {
    val tmp = java.nio.ByteBuffer.wrap(data)
    Buffer.copyBuffer(tmp, buffer, data.length)
  }

  /** Transfers data from GPU back to CPU. */
  def downloadData(buffer: Buffer): Array[Byte] = {
    val out = new Array[Byte](buffer.size)
    buffer.get(out)
    out
  }

  /** Clean up all Vulkan resources. */
  override def close(): Unit = {
    vkContext.destroy()
  }
}

object ComputeContext {
  /** Creates a context and closes it automatically. */
  def withContext[T](enableValidation: Boolean = false)(block: ComputeContext => T): T =
    Using.resource(new ComputeContext(enableValidation)) { ctx =>
      block(ctx)
    }
}