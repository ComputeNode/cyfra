package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value.Float32
import io.computenode.cyfra.vulkan.memory.Buffer
import io.computenode.cyfra.runtime.GContext
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.util.vma.Vma.*

import java.nio.ByteBuffer

class FloatMem(val size: Int, val vulkanBuffer: Buffer) extends RamGMem[Float32, Float]:
  def toArray(using context: GContext): Array[Float] =
    val allocator = context.vkContext.allocator
    val commandPool = context.vkContext.commandPool
    val bufferSize = size.toLong * FloatMem.FloatSize

    val stagingBuffer = new Buffer(
      bufferSize.toInt,
      VK_BUFFER_USAGE_TRANSFER_DST_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      VMA_MEMORY_USAGE_GPU_TO_CPU,
      allocator
    )

    Buffer.copyBuffer(vulkanBuffer, stagingBuffer, bufferSize, commandPool).block().close()
    
    val result = stagingBuffer.map { byteBuffer =>
      val floatBuffer = byteBuffer.asFloatBuffer()
      val arr = new Array[Float](size)
      floatBuffer.get(arr)
      arr
    }

    stagingBuffer.destroy()
    result

  def cleanup(): Unit =
    vulkanBuffer.destroy()

object FloatMem {
  val FloatSize = 4

  def apply(floats: Array[Float])(using context: GContext): FloatMem =
    val size = floats.length
    val bufferSize = size.toLong * FloatSize
    val allocator = context.vkContext.allocator
    val commandPool = context.vkContext.commandPool

    val stagingBuffer = new Buffer(
      bufferSize.toInt,
      VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      VMA_MEMORY_USAGE_CPU_ONLY,
      allocator
    )

    stagingBuffer.map { byteBuffer =>
      byteBuffer.asFloatBuffer().put(floats)
    }

    val deviceBuffer = new Buffer(
      bufferSize.toInt,
      VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      0, 
      VMA_MEMORY_USAGE_GPU_ONLY,
      allocator
    )

    Buffer.copyBuffer(stagingBuffer, deviceBuffer, bufferSize, commandPool).block().close()
    stagingBuffer.destroy()

    new FloatMem(size, deviceBuffer)

  def apply(size: Int)(using context: GContext): FloatMem = 
    val bufferSize = size.toLong * FloatSize
    val allocator = context.vkContext.allocator
    val deviceBuffer = new Buffer(
      bufferSize.toInt,
      VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      0,
      VMA_MEMORY_USAGE_GPU_ONLY,
      allocator
    )
    new FloatMem(size, deviceBuffer)
}
