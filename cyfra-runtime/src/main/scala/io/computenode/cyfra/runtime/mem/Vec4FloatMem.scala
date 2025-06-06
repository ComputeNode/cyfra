package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value.{Float32, Vec4}
import io.computenode.cyfra.runtime.mem.GMem.fRGBA
import io.computenode.cyfra.vulkan.memory.Buffer
import io.computenode.cyfra.runtime.GContext
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.util.vma.Vma.*

import java.nio.ByteBuffer

class Vec4FloatMem(val size: Int, val vulkanBuffer: Buffer) extends RamGMem[Vec4[Float32], fRGBA]:
  def toArray(using context: GContext): Array[fRGBA] = {
    val allocator = context.vkContext.allocator
    val commandPool = context.vkContext.commandPool
    val bufferSize = size.toLong * Vec4FloatMem.Vec4FloatSize

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
      val arr = new Array[fRGBA](size)
      for (i <- 0 until size)
        arr(i) = (floatBuffer.get(), floatBuffer.get(), floatBuffer.get(), floatBuffer.get())
      arr
    }
    stagingBuffer.destroy()
    result
  }

  def cleanup(): Unit =
    vulkanBuffer.destroy()

object Vec4FloatMem:
  val Vec4FloatSize = 16

  def apply(vecs: Array[fRGBA])(using context: GContext): Vec4FloatMem = {
    val size = vecs.length
    val bufferSize = size.toLong * Vec4FloatSize
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
      val floatBuffer = byteBuffer.asFloatBuffer()
      vecs.foreach { case (x, y, z, a) =>
        floatBuffer.put(x)
        floatBuffer.put(y)
        floatBuffer.put(z)
        floatBuffer.put(a)
      }
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

    new Vec4FloatMem(size, deviceBuffer)
  }

  def apply(size: Int)(using context: GContext): Vec4FloatMem =
    val bufferSize = size.toLong * Vec4FloatSize
    val allocator = context.vkContext.allocator
    val deviceBuffer = new Buffer(
      bufferSize.toInt,
      VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      0,
      VMA_MEMORY_USAGE_GPU_ONLY,
      allocator
    )
    new Vec4FloatMem(size, deviceBuffer)
