package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.command.Fence
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.pushStack
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.vulkan.VK10.{vkGetDeviceQueue, vkQueueSubmit}
import org.lwjgl.vulkan.{VkQueue, VkSubmitInfo}

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] class Queue(val familyIndex: Int, queueIndex: Int, device: Device) extends VulkanObject:
  private val queue: VkQueue = pushStack: stack =>
    val pQueue = stack.callocPointer(1)
    vkGetDeviceQueue(device.get, familyIndex, queueIndex, pQueue)
    new VkQueue(pQueue.get(0), device.get)

  def get: VkQueue = queue

  protected def close(): Unit = ()
