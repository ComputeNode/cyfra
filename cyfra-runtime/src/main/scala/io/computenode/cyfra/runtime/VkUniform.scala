package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GUniform
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import izumi.reflect.Tag
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.*

class VkUniform[T <: Value: {Tag, FromExpr}] private (val underlying: Buffer) extends GUniform[T]:
  val sizeOfT: Int = typeStride(summon[Tag[T]])

object VkUniform:
  private final val UsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT |
    VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT

  def apply[T <: Value: {Tag, FromExpr}]()(using Allocator): VkUniform[T] =
    val sizeOfT = typeStride(summon[Tag[T]])
    val buffer = new Buffer.DeviceBuffer(sizeOfT, UsageFlags)
    new VkUniform[T](buffer)
