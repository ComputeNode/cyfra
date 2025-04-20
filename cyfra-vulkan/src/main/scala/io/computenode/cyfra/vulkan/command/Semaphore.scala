package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkSemaphoreCreateInfo

import scala.util.Using

/** @author
  *   MarconZet Created 30.10.2019
  */
private[cyfra] class Semaphore(device: Device) extends VulkanObjectHandle {
  protected val handle: Long = pushStack { stack =>
    val semaphoreCreateInfo = VkSemaphoreCreateInfo
      .calloc(stack)
      .sType$Default()
    val pointer = stack.callocLong(1)
    check(vkCreateSemaphore(device.get, semaphoreCreateInfo, null, pointer), "Failed to create semaphore")
    pointer.get()
  }

  def close(): Unit =
    vkDestroySemaphore(device.get, handle, null)

}
