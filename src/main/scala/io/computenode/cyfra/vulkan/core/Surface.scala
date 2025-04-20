package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.glfw.GLFWVulkan
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.glfw.GLFW
import org.lwjgl.vulkan.{VkPhysicalDevice, VkSurfaceCapabilitiesKHR}

/** 
  * Class that encapsulates a Vulkan surface (VkSurfaceKHR)
  */
private[cyfra] class Surface(instance: Instance, windowHandle: Long) extends VulkanObjectHandle {

  protected val handle: Long = pushStack { stack =>
    val pSurface = stack.callocLong(1)
    check(
      GLFWVulkan.glfwCreateWindowSurface(instance.get, windowHandle, null, pSurface),
      "Failed to create window surface"
    )
    pSurface.get(0)
  }

  /** Get surface capabilities for a physical device
    *
    * @param physicalDevice The physical device to query capabilities for
    * @return Surface capabilities structure
    */
  def getCapabilities(physicalDevice: VkPhysicalDevice): VkSurfaceCapabilitiesKHR = pushStack { stack =>
    val capabilities = VkSurfaceCapabilitiesKHR.calloc(stack)
    check(
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, handle, capabilities),
      "Failed to get surface capabilities"
    )
    capabilities
  }

  /** Check if the physical device supports presentation on this surface
    * 
    * @param physicalDevice The physical device to check
    * @param queueFamilyIndex The queue family index to check
    * @return True if presentation is supported
    */
  def supportsPresentationFrom(physicalDevice: VkPhysicalDevice, queueFamilyIndex: Int): Boolean = pushStack { stack =>
    val pSupported = stack.callocInt(1)
    check(
      vkGetPhysicalDeviceSurfaceSupportKHR(physicalDevice, queueFamilyIndex, handle, pSupported),
      "Failed to check presentation support"
    )
    pSupported.get(0) == VK_TRUE
  }

  override protected def close(): Unit = {
    vkDestroySurfaceKHR(instance.get, handle, null)
  }
}