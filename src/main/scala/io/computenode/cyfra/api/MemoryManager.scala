package io.computenode.cyfra.api

import io.computenode.cyfra.vulkan.memory.{Buffer, Allocator}
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import org.lwjgl.util.vma.Vma.VMA_MEMORY_USAGE_GPU_ONLY
import org.lwjgl.vulkan.VK10._
import java.nio.ByteBuffer

import scala.collection.mutable

/**
  * Central place for allocating, pooling, and synchronizing GPU memory buffers.
  */
object MemoryManager:

  // Minimal alignment to ensure GPU-friendly sizes.
  private def alignSize(size: Long, alignment: Long): Long =
    ((size + alignment - 1) / alignment) * alignment

  /**
    * A simple memory pool that auto-allocates and reuses freed buffers.
    *
    * @param allocator underlying Vulkan memory allocator
    * @param chunkSize base chunk size for new allocations
    */
  class MemoryPool(allocator: Allocator, chunkSize: Long = 65536):
    private val freeBuffers = mutable.Stack[Buffer]()

    /** Retrieve an aligned buffer from the pool, or create a new one if needed. */
    def acquireBuffer(size: Long, usage: Int = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT): Buffer =
      val aligned = alignSize(size, 16) // 16-byte alignment
      freeBuffers.find(_.size == aligned) match
        case Some(buf) =>
          freeBuffers.pop()
          buf
        case None =>
          new Buffer(aligned.toInt, usage, 0, VMA_MEMORY_USAGE_GPU_ONLY, allocator)

    /** Return a buffer to the pool for later reuse. */
    def releaseBuffer(buf: Buffer): Unit =
      freeBuffers.push(buf)

    /** Destroy all pooled buffers. */
    def close(): Unit =
      freeBuffers.foreach(_.destroy())
      freeBuffers.clear()

  /**
    * Type-safe wrapper for float array buffer management.
    */
  def createFloatBuffer(data: Array[Float], pool: MemoryPool): Buffer =
    val neededBytes = data.length.toLong * 4
    val buf = pool.acquireBuffer(neededBytes, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
    val byteBuffer = ByteBuffer.wrap(data.map(java.lang.Float.floatToIntBits(_).toByte))
    Buffer.copyBuffer(byteBuffer, buf, neededBytes)
    buf

  /**
    * Type-safe wrapper for int array buffer management.
    */
  def createIntBuffer(data: Array[Int], pool: MemoryPool): Buffer =
    val neededBytes = data.length.toLong * 4
    val buf = pool.acquireBuffer(neededBytes, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
    val byteBuffer = ByteBuffer.allocate(data.length * 4)
    data.foreach(i => byteBuffer.putInt(i))
    byteBuffer.flip()
    Buffer.copyBuffer(byteBuffer, buf, neededBytes)
    buf

  /**
    * Simple fence utility for synchronizing GPU operations.
    */
  def waitOnFence(fence: Fence): Unit =
    fence.block().destroy()