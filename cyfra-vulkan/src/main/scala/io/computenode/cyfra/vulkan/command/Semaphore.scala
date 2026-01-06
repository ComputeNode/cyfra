package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK12.*
import org.lwjgl.vulkan.{VkSemaphoreCreateInfo, VkSemaphoreSignalInfo, VkSemaphoreTypeCreateInfo, VkSemaphoreWaitInfo}

import scala.concurrent.duration.Duration

/** @author
  *   MarconZet Created 30.10.2019
  */
private[cyfra] class Semaphore()(using device: Device) extends VulkanObjectHandle:
  protected val handle: Long = pushStack: stack =>
    val timelineCI = VkSemaphoreTypeCreateInfo
      .calloc(stack)
      .sType$Default()
      .semaphoreType(VK_SEMAPHORE_TYPE_TIMELINE)
      .initialValue(0)

    val semaphoreCI = VkSemaphoreCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(timelineCI)
      .flags(0)

    val pointer = stack.callocLong(1)
    check(vkCreateSemaphore(device.get, semaphoreCI, null, pointer), "Failed to create semaphore")
    pointer.get()

  def setValue(value: Long): Unit = pushStack: stack =>
    val signalI = VkSemaphoreSignalInfo
      .calloc(stack)
      .sType$Default()
      .semaphore(handle)
      .value(value)

    check(vkSignalSemaphore(device.get, signalI), "Failed to signal semaphore")

  def getValue: Long = pushStack: stack =>
    val pValue = stack.callocLong(1)
    check(vkGetSemaphoreCounterValue(device.get, handle, pValue), "Failed to get semaphore value")
    pValue.get()

  def waitValue(value: Long, timeout: Duration = Duration.fromNanos(Long.MaxValue)): Unit = pushStack: stack =>
    val waitI = VkSemaphoreWaitInfo
      .calloc(stack)
      .sType$Default()
      .pSemaphores(stack.longs(handle))
      .pValues(stack.longs(value))

    check(vkWaitSemaphores(device.get, waitI, timeout.toNanos), "Failed to wait for semaphore")

  def close(): Unit =
    vkDestroySemaphore(device.get, handle, null)