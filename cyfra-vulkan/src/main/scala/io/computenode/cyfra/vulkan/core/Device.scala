package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext.ValidationLayer
import Device.MacOsExtension
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.{VulkanObject, VulkanObjectHandle}
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.KHRPortabilitySubset.VK_KHR_PORTABILITY_SUBSET_EXTENSION_NAME
import org.lwjgl.vulkan.KHRSynchronization2.VK_KHR_SYNCHRONIZATION_2_EXTENSION_NAME
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK11.*

import java.nio.ByteBuffer
import scala.jdk.CollectionConverters.given

/** @author
  *   MarconZet Created 13.04.2020
  */

object Device:
  final val MacOsExtension = VK_KHR_PORTABILITY_SUBSET_EXTENSION_NAME

private[cyfra] class Device(instance: Instance, physicalDevice: PhysicalDevice) extends VulkanObject[VkDevice]:
  protected val handle: VkDevice = pushStack: stack =>
    val (queueFamily, queueCount) = physicalDevice.selectComputeQueueFamily
    val pQueueCreateInfo = VkDeviceQueueCreateInfo.calloc(1, stack)
    pQueueCreateInfo
      .get(0)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .queueFamilyIndex(queueFamily)
      .pQueuePriorities(stack.callocFloat(queueCount))

    val extensions = Seq(MacOsExtension).filter(physicalDevice.deviceExtensionsSet)
    val ppExtensionNames = stack.callocPointer(extensions.length)
    extensions.foreach(extension => ppExtensionNames.put(stack.ASCII(extension)))
    ppExtensionNames.flip()

    val sync2 = VkPhysicalDeviceSynchronization2Features
      .calloc(stack)
      .sType$Default()
      .synchronization2(true)

    val pCreateInfo = VkDeviceCreateInfo
      .create()
      .sType$Default()
      .pNext(sync2)
      .pQueueCreateInfos(pQueueCreateInfo)
      .ppEnabledExtensionNames(ppExtensionNames)

    if instance.enabledLayers.nonEmpty then
      val ppValidationLayers = stack.callocPointer(instance.enabledLayers.length)
      instance.enabledLayers.foreach: layer =>
        ppValidationLayers.put(stack.ASCII(layer))
      pCreateInfo.ppEnabledLayerNames(ppValidationLayers.flip())

    val pDevice = stack.callocPointer(1)
    check(vkCreateDevice(physicalDevice.get, pCreateInfo, null, pDevice), "Failed to create device")
    val device = new VkDevice(pDevice.get(0), physicalDevice.get, pCreateInfo)
    device

  def getQueues: Seq[Queue] =
    val (queueFamily, queueCount) = physicalDevice.selectComputeQueueFamily
    (0 until queueCount).map(new Queue(queueFamily, _, this))

  override protected def close(): Unit =
    vkDestroyDevice(handle, null)
