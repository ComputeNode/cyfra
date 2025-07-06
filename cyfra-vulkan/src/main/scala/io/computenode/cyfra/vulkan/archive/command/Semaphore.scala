package io.computenode.cyfra.vulkan.archive.command

import io.computenode.cyfra.vulkan.archive.core.Device
import io.computenode.cyfra.vulkan.archive.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.archive.util.VulkanObjectHandle
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkSemaphoreCreateInfo

/** @author
  *   MarconZet Created 30.10.2019
  */
private[cyfra] class Semaphore(device: Device) extends VulkanObjectHandle:
  protected val handle: Long = pushStack: stack =>
    val semaphoreCreateInfo = VkSemaphoreCreateInfo
      .calloc(stack)
      .sType$Default()
    val pointer = stack.callocLong(1)
    check(vkCreateSemaphore(device.get, semaphoreCreateInfo, null, pointer), "Failed to create semaphore")
    pointer.get()

  def close(): Unit =
    vkDestroySemaphore(device.get, handle, null)
