package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObject}
import org.lwjgl.glfw.{GLFW, GLFWFramebufferSizeCallback}
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.KHRSurface.*

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/** Class that manages multiple Vulkan surfaces
  * 
  * Provides centralized management for all active surfaces, including
  * creation, destruction, and event routing.
  *
  * @param context The Vulkan context to use for surface operations
  */
private[cyfra] class SurfaceManager(val context: VulkanContext) extends VulkanObject {
  
  // Registry to track active surfaces by window handle
  private val activeSurfaces: mutable.Map[Long, Surface] = mutable.Map.empty
  
  // Track resize callbacks to prevent garbage collection
  private val resizeCallbacks: mutable.Map[Long, GLFWFramebufferSizeCallback] = mutable.Map.empty
  
  /** Create and register a new surface for a window handle
    * 
    * @param windowHandle The GLFW window handle
    * @return The created Surface
    */
  def createSurface(windowHandle: Long): Surface = {
    if (activeSurfaces.contains(windowHandle)) {
      throw new VulkanAssertionError(s"Surface already exists for window $windowHandle", -1)
    }
    
    val surface = SurfaceFactory.create(context, windowHandle)
    activeSurfaces(windowHandle) = surface
    
    // Set up resize callback for this window
    setupResizeCallback(windowHandle)
    
    surface
  }
  
  /** Get a surface by window handle
    * 
    * @param windowHandle The GLFW window handle
    * @return The Surface if found
    */
  def getSurface(windowHandle: Long): Option[Surface] = {
    activeSurfaces.get(windowHandle)
  }
  
  /** Remove and destroy a surface
    * 
    * @param windowHandle The GLFW window handle
    * @return True if the surface was found and removed
    */
  def removeSurface(windowHandle: Long): Boolean = {
    activeSurfaces.remove(windowHandle).map { surface =>
      surface.destroy()
      
      // Clean up resize callback
      resizeCallbacks.remove(windowHandle).foreach { callback =>
        callback.free()
      }
      
      true
    }.getOrElse(false)
  }
  
  /** Check if a surface exists for a window
    * 
    * @param windowHandle The GLFW window handle
    * @return True if a surface exists for this window
    */
  def hasSurface(windowHandle: Long): Boolean = {
    activeSurfaces.contains(windowHandle)
  }
  
  /** Get count of active surfaces
    * 
    * @return Number of active surfaces
    */
  def getActiveSurfaceCount: Int = {
    activeSurfaces.size
  }
  
  /** Get all active surfaces
    * 
    * @return Collection of active surfaces
    */
  def getAllSurfaces: Iterable[Surface] = {
    activeSurfaces.values
  }
  
  /** Get all window handles with active surfaces
    * 
    * @return Collection of window handles
    */
  def getAllWindowHandles: Iterable[Long] = {
    activeSurfaces.keys
  }
  
  /** Setup framebuffer resize callback for a window
    * 
    * @param windowHandle The GLFW window handle
    */
  private def setupResizeCallback(windowHandle: Long): Unit = {
    val callback = GLFWFramebufferSizeCallback.create { (window, width, height) =>
      if (width > 0 && height > 0) {
        // Handle resize event - application would typically react here
        // For example, recreate swapchain if this were a graphics application
        onWindowResize(window, width, height)
      }
    }
    
    GLFW.glfwSetFramebufferSizeCallback(windowHandle, callback)
    resizeCallbacks(windowHandle) = callback
  }
  
  /** Event handler for window resize
    * 
    * @param windowHandle The GLFW window handle
    * @param width The new width
    * @param height The new height
    */
  def onWindowResize(windowHandle: Long, width: Int, height: Int): Unit = {
    // This would typically be overridden or provide a way for the application 
    // to register its own resize handlers
  }
  
  /** Handle window close event
    * 
    * @param windowHandle The GLFW window handle
    */
  def onWindowClose(windowHandle: Long): Unit = {
    removeSurface(windowHandle)
  }
  
  /** Check if a specific queue family on a physical device supports presentation to any active surface
    * 
    * @param physicalDevice The physical device
    * @param queueFamilyIndex The queue family index
    * @return True if presentation is supported on any surface
    */
  def isQueueFamilySupportedForAny(physicalDevice: Long, queueFamilyIndex: Int): Boolean = {
    if (activeSurfaces.isEmpty) {
      return false
    }
    
    activeSurfaces.values.exists { surface => 
      Try(context.isQueueFamilyPresentSupported(queueFamilyIndex, surface)) match {
        case Success(isSupported) => isSupported
        case Failure(_) => false
      }
    }
  }
  
  /** Collect surface capabilities for all active surfaces
    * 
    * @return Map of window handles to surface capabilities
    */
  def getAllSurfaceCapabilities: Map[Long, SurfaceCapabilities] = {
    activeSurfaces.map { case (windowHandle, surface) =>
      windowHandle -> context.getSurfaceCapabilities(surface)
    }.toMap
  }
  
  /** Close and destroy all resources */
  protected def close(): Unit = {
    // Clean up all active surfaces
    activeSurfaces.foreach { case (windowHandle, surface) =>
      Try(surface.destroy())
      
      // Clean up callbacks
      resizeCallbacks.get(windowHandle).foreach { callback =>
        callback.free()
      }
    }
    
    activeSurfaces.clear()
    resizeCallbacks.clear()
  }
}