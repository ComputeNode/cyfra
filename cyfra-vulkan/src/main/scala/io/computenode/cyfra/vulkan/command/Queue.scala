package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.util.Util.pushStack
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.PointerBuffer
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.VK10.{vkGetDeviceQueue, vkQueueSubmit}
import org.lwjgl.vulkan.{VkQueue, VkSubmitInfo}

import scala.util.Using

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] class Queue(val familyIndex: Int, queueIndex: Int, device: Device) extends VulkanObject {
  private val queue: VkQueue = pushStack { stack =>
    val pQueue = stack.callocPointer(1)
    vkGetDeviceQueue(device.get, familyIndex, queueIndex, pQueue)
    new VkQueue(pQueue.get(0), device.get)
  }

  def submit(submitInfo: VkSubmitInfo, fence: Fence): Int = this.synchronized {
    vkQueueSubmit(queue, submitInfo, fence.get)
  }

  def get: VkQueue = queue

  protected def close(): Unit = ()
}
