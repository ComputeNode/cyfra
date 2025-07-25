package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer}
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import izumi.reflect.Tag
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.{VK_BUFFER_USAGE_STORAGE_BUFFER_BIT, VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_BUFFER_USAGE_TRANSFER_SRC_BIT}

class VkBuffer[T <: Value: {Tag, FromExpr}] private (var length: Int, val underlying: Buffer) extends GBuffer[T]:
  val sizeOfT: Int = typeStride(summon[Tag[T]])

object VkBuffer:
  private final val Padding = 64
  private final val UsageFlags = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT

  def apply[T <: Value: {Tag, FromExpr}](length: Int)(using Allocator): VkBuffer[T] =
    val sizeOfT = typeStride(summon[Tag[T]])
    val size = (length * sizeOfT + Padding - 1) / Padding * Padding
    val buffer = new Buffer.DeviceBuffer(size, UsageFlags)
    new VkBuffer[T](length, buffer)
