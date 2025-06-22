package io.computenode.cyfra.rtrp.window.platform

import io.computenode.cyfra.rtrp.window.core._
import org.lwjgl.glfw.{GLFW, GLFWErrorCallback}
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.glfw.GLFWVulkan.glfwVulkanSupported
import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import java.util.concurrent.atomic.{AtomicLong, AtomicBoolean}

// GLFW implementation of the WindowSystem trait
class GLFWWindowSystem extends WindowSystem {
  
  private val windowIdGenerator = new AtomicLong(1L)
  private val initialized = new AtomicBoolean(false)
  private val activeWindows = mutable.Map[WindowId, GLFWWindow]()
  private var errorCallback: GLFWErrorCallback = _
  
  override def initialize(): Try[Unit] = Try {
    if (initialized.get()) {
      throw new IllegalStateException("WindowSystem is already initialized")
    }
    
    errorCallback = GLFWErrorCallback.createPrint(System.err)
    GLFW.glfwSetErrorCallback(errorCallback)
    
    if (!GLFW.glfwInit()) {
      throw new RuntimeException("Failed to initialize GLFW")
    }
    
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)
    
    if (!glfwVulkanSupported()) {
      throw new RuntimeException("GLFW: Vulkan is not supported on this system")
    }
    
    // Register shutdown hook for cleanup
    sys.addShutdownHook {
      if (initialized.get()) {
        shutdown()
      }
    }
    
    initialized.set(true)
  }
  
  override def shutdown(): Try[Unit] = Try {
    if (!initialized.get()) {
      return Success(())
    }
    
    activeWindows.values.foreach(_.destroy())
    activeWindows.clear()
    
    GLFW.glfwTerminate()
    
    if (errorCallback != null) {
      errorCallback.free()
    }
    
    initialized.set(false)
  }
  
  override def createWindow(config: WindowConfig): Try[Window] = Try {
    if (!initialized.get()) {
      throw new IllegalStateException("WindowSystem not initialized")
    }
    
    applyWindowHints(config)
    
    val windowPtr = GLFW.glfwCreateWindow(
      config.width, 
      config.height, 
      config.title, 
      if (config.fullscreen) GLFW.glfwGetPrimaryMonitor() else NULL,
      NULL
    )
    
    if (windowPtr == NULL) {
      throw new RuntimeException("Failed to create GLFW window")
    }
    
    val windowId = WindowId(windowIdGenerator.getAndIncrement())
    
    val window = new GLFWWindow(windowId, windowPtr, config, this)
    
    activeWindows.put(windowId, window)
    
    setupWindowPosition(windowPtr, config)
    
    GLFW.glfwShowWindow(windowPtr)
    
    window
  }
  
  override def destroyWindow(window: Window): Try[Unit] = Try {
    window match {
      case glfwWindow: GLFWWindow =>
        activeWindows.remove(glfwWindow.id)
        glfwWindow.destroy()
      case _ =>
        throw new IllegalArgumentException("Window is not a GLFW window")
    }
  }
  
  override def pollEvents(): Try[List[WindowEvent]] = Try {
    if (!initialized.get()) {
      throw new IllegalStateException("WindowSystem not initialized")
    }
    
    GLFW.glfwPollEvents()
    
    val allEvents = activeWindows.values.flatMap(_.pollEvents()).toList
    allEvents
  }
  
  override def getActiveWindows(): List[Window] = {
    activeWindows.values.toList
  }
  
  override def findWindow(id: WindowId): Option[Window] = {
    activeWindows.get(id)
  }
  
  override def isInitialized: Boolean = initialized.get()
  
  // Internal method to remove window from tracking (called by GLFWWindow)
  private[platform] def unregisterWindow(windowId: WindowId): Unit = {
    activeWindows.remove(windowId)
  }
  
  private def applyWindowHints(config: WindowConfig): Unit = {
    // Core window hints
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)
    GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, if (config.resizable) GLFW.GLFW_TRUE else GLFW.GLFW_FALSE)
    GLFW.glfwWindowHint(GLFW.GLFW_DECORATED, if (config.decorated) GLFW.GLFW_TRUE else GLFW.GLFW_FALSE)
    GLFW.glfwWindowHint(GLFW.GLFW_VISIBLE, GLFW.GLFW_FALSE) // We'll show it manually
    
    // MSAA samples
    if (config.samples > 1) {
      GLFW.glfwWindowHint(GLFW.GLFW_SAMPLES, config.samples)
    }
    
    // Platform-specific hints
    val osName = System.getProperty("os.name").toLowerCase
    if (osName.contains("mac")) {
      GLFW.glfwWindowHint(GLFW.GLFW_COCOA_GRAPHICS_SWITCHING, GLFW.GLFW_TRUE)
    } else if (osName.contains("win")) {
      GLFW.glfwWindowHint(GLFW.GLFW_SCALE_TO_MONITOR, GLFW.GLFW_TRUE)
    }
  }
  
  private def setupWindowPosition(windowPtr: Long, config: WindowConfig): Unit = {
    config.position match {
      case Some(WindowPosition.Centered) =>
        val monitor = GLFW.glfwGetPrimaryMonitor()
        val vidMode = GLFW.glfwGetVideoMode(monitor)
        val centerX = (vidMode.width() - config.width) / 2
        val centerY = (vidMode.height() - config.height) / 2
        GLFW.glfwSetWindowPos(windowPtr, centerX, centerY)
        
      case Some(WindowPosition.Fixed(x, y)) =>
        GLFW.glfwSetWindowPos(windowPtr, x, y)
        
      case Some(WindowPosition.Default) | None =>
        // Let GLFW decide the position
    }
  }
}