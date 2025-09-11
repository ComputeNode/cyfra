package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkFenceCreateInfo

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] class Fence(flags: Int = 0)(using device: Device) extends VulkanObjectHandle:
  protected val handle: Long = pushStack(stack =>
    val fenceInfo = VkFenceCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(flags)

    val pFence = stack.callocLong(1)
    check(vkCreateFence(device.get, fenceInfo, null, pFence), "Failed to create fence")
    pFence.get()
  )

  override def close(): Unit =
    vkDestroyFence(device.get, handle, null)

  def isSignaled: Boolean =
    val result = vkGetFenceStatus(device.get, handle)
    if !(result == VK_SUCCESS || result == VK_NOT_READY) then throw new VulkanAssertionError("Failed to get fence status", result)
    result == VK_SUCCESS

  def reset(): Fence =
    vkResetFences(device.get, handle)
    this

  def block(): Fence =
    block(Long.MaxValue)
    this

  def block(timeout: Long): Boolean =
    val err = vkWaitForFences(device.get, handle, true, timeout)
    if err != VK_SUCCESS && err != VK_TIMEOUT then throw new VulkanAssertionError("Failed to wait for fences", err)
    err == VK_SUCCESS;
