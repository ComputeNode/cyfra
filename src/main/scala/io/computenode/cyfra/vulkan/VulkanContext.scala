package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.command.{CommandPool, Queue, StandardCommandPool}
import io.computenode.cyfra.vulkan.core.{DebugCallback, Device, Instance, Surface, SurfaceCapabilities}
import io.computenode.cyfra.vulkan.memory.{Allocator, DescriptorPool}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkSurfaceCapabilitiesKHR, VkSurfaceFormatKHR}

import scala.jdk.CollectionConverters.given

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] object VulkanContext {
  final val ValidationLayer: String = "VK_LAYER_KHRONOS_validation"
  final val SyncLayer: String = "VK_LAYER_KHRONOS_synchronization2"
}

private[cyfra] class VulkanContext(val enableValidationLayers: Boolean = false) {
  private val sdkPresent = org.lwjgl.system.Configuration.VULKAN_LIBRARY_NAME.get() != null
  private val validationLayers = enableValidationLayers && sdkPresent

  val instance: Instance = new Instance(validationLayers)
  val debugCallback: DebugCallback = if (validationLayers) new DebugCallback(instance) else null
  val device: Device = new Device(instance)
  val computeQueue: Queue = new Queue(device.computeQueueFamily, 0, device)
  val allocator: Allocator = new Allocator(instance, device)
  val descriptorPool: DescriptorPool = new DescriptorPool(device)
  val commandPool: CommandPool = new StandardCommandPool(device, computeQueue)

  /** Get surface capabilities for a surface
   *
   * @param surface The surface to query capabilities for
   * @return A SurfaceCapabilities object containing the queried capabilities
   */
  def getSurfaceCapabilities(surface: Surface): SurfaceCapabilities = {
    new SurfaceCapabilities(device.physicalDevice, surface)
  }
  
  /** Get available surface formats for a surface
   *
   * @param surface The surface to query formats for
   * @return A list of supported surface formats
   */
  def getSurfaceFormats(surface: Surface): List[VkSurfaceFormatKHR] = pushStack { stack =>
    val countPtr = stack.callocInt(1)
    check(
      vkGetPhysicalDeviceSurfaceFormatsKHR(device.physicalDevice, surface.get, countPtr, null),
      "Failed to get surface format count"
    )
    
    val count = countPtr.get(0)
    if (count == 0) {
      return List.empty
    }
    
    val surfaceFormats = VkSurfaceFormatKHR.calloc(count, stack)
    check(
      vkGetPhysicalDeviceSurfaceFormatsKHR(device.physicalDevice, surface.get, countPtr, surfaceFormats),
      "Failed to get surface formats"
    )
    
    surfaceFormats.iterator().asScala.toList
  }
  
  /** Get available presentation modes for a surface
   *
   * @param surface The surface to query presentation modes for
   * @return A list of supported presentation modes
   */
  def getPresentModes(surface: Surface): List[Int] = pushStack { stack =>
    val countPtr = stack.callocInt(1)
    check(
      vkGetPhysicalDeviceSurfacePresentModesKHR(device.physicalDevice, surface.get, countPtr, null),
      "Failed to get presentation mode count"
    )
    
    val count = countPtr.get(0)
    if (count == 0) {
      return List.empty
    }
    
    val presentModes = stack.callocInt(count)
    check(
      vkGetPhysicalDeviceSurfacePresentModesKHR(device.physicalDevice, surface.get, countPtr, presentModes),
      "Failed to get presentation modes"
    )
    
    val result = collection.mutable.ListBuffer[Int]()
    for (i <- 0 until count) {
      result += presentModes.get(i)
    }
    result.toList
  }
  
  /** Check if a queue family supports presentation to a surface
   *
   * @param queueFamilyIndex The queue family index to check
   * @param surface The surface to check presentation support for
   * @return Whether the queue family supports presentation to the surface
   */
  def isQueueFamilyPresentSupported(queueFamilyIndex: Int, surface: Surface): Boolean = pushStack { stack =>
    val pSupported = stack.callocInt(1)
    check(
      vkGetPhysicalDeviceSurfaceSupportKHR(device.physicalDevice, queueFamilyIndex, surface.get, pSupported),
      "Failed to check queue family presentation support"
    )
    pSupported.get(0) == VK_TRUE
  }

  def destroy(): Unit = {
    commandPool.destroy()
    descriptorPool.destroy()
    allocator.destroy()
    computeQueue.destroy()
    device.destroy()
    if (validationLayers)
      debugCallback.destroy()
    instance.destroy()
  }
}
