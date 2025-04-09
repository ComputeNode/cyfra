package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.glfw.{GLFW, GLFWVulkan}
import org.lwjgl.system.{Platform, MemoryStack}
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.vulkan.KHRSurface.VK_KHR_SURFACE_EXTENSION_NAME
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.EXTMetalSurface

/** Factory object to create Vulkan surfaces using GLFW
  * 
  * @author
  *   Created based on project conventions
  */
private[cyfra] object SurfaceFactory {
  // Extension name constants
  private val WIN32_EXTENSION_NAME = "VK_KHR_win32_surface"
  private val XCB_EXTENSION_NAME = "VK_KHR_xcb_surface"
  private val XLIB_EXTENSION_NAME = "VK_KHR_xlib_surface"
  private val WAYLAND_EXTENSION_NAME = "VK_KHR_wayland_surface"
  private val METAL_EXTENSION_NAME = "VK_EXT_metal_surface"
  
  /** Creates a Vulkan surface using GLFW's cross-platform method
    *
    * @param context The Vulkan context containing the instance
    * @param windowHandle The handle to the window to create the surface for
    * @return A new Surface object
    */
  def create(context: VulkanContext, windowHandle: Long): Surface = {
    val instance = context.instance
    
    if (!checkSurfaceExtensionSupport(instance)) {
      throw new VulkanAssertionError("Required surface extensions not available", -1)
    }
    
    // Create surface using GLFW - this works on all platforms
    new Surface(instance, windowHandle)
  }
  
  /** Checks whether the required surface extensions are available for the current platform
    *
    * @param instance The Vulkan instance to check against
    * @return true if all required extensions are available
    */
  def checkSurfaceExtensionSupport(instance: Instance): Boolean = {
    val requiredExtensions = getPlatformSpecificExtensions()
    requiredExtensions.forall(ext => Instance.extensions.contains(ext))
  }
  
  /** Gets the platform-specific surface extensions required
    *
    * @return A sequence of extension names
    */
  def getPlatformSpecificExtensions(): Seq[String] = {
    val platformExtensions = Platform.get() match {
      case Platform.WINDOWS => 
        Seq(WIN32_EXTENSION_NAME)
      case Platform.LINUX => 
        // On Linux we might need one of these based on window system
        Seq(XCB_EXTENSION_NAME, XLIB_EXTENSION_NAME, WAYLAND_EXTENSION_NAME)
      case Platform.MACOSX => 
        Seq(METAL_EXTENSION_NAME)
      case _ => 
        Seq.empty
    }
    
    // Always include the base surface extension
    VK_KHR_SURFACE_EXTENSION_NAME +: platformExtensions
  }
}