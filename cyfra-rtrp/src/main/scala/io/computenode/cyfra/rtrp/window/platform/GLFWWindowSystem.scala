package io.computenode.cyfra.rtrp.window.platform

import io.computenode.cyfra.rtrp.window.core.*
import io.computenode.cyfra.rtrp.window.*
import org.lwjgl.glfw.*
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.glfw.GLFWVulkan.glfwVulkanSupported
import scala.util.*
import scala.collection.mutable
import java.util.concurrent.atomic.*

// GLFW implementation of the WindowSystem trait
class GLFWWindowSystem extends WindowSystem:

  private val windowIdGenerator = new AtomicLong(1L)
  private val initialized = new AtomicBoolean(false)
  private val activeWindows = mutable.Map[WindowId, GLFWWindow]()
  private var errorCallback: GLFWErrorCallback = _

  override def initialize(): Try[Unit] = Try:
    if initialized.get() then throw WindowSystemInitializationException("WindowSystem is already initialized")

    errorCallback = GLFWErrorCallback.createPrint(System.err)
    GLFW.glfwSetErrorCallback(errorCallback)

    if !GLFW.glfwInit() then throw GLFWException("Failed to initialize GLFW")

    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)

    if !glfwVulkanSupported() then throw VulkanNotSupportedException("GLFW: Vulkan is not supported on this system")

    // Register shutdown hook for cleanup
    sys.addShutdownHook:
      if initialized.get() then shutdown()

    initialized.set(true)

  override def shutdown(): Try[Unit] = Try:
    if !initialized.get() then return Success(())

    try
      activeWindows.values.foreach(_.destroy())
      activeWindows.clear()

      GLFW.glfwTerminate()

      if errorCallback != null then errorCallback.free()

      initialized.set(false)
    catch
      case e: Exception =>
        throw WindowSystemShutdownException("Failed to shutdown window system", e)

  override def createWindow(config: WindowConfig): Try[Window] = Try:
    if !initialized.get() then throw WindowSystemNotInitializedException()

    applyWindowHints(config)

    val windowPtr =
      GLFW.glfwCreateWindow(config.width, config.height, config.title, if config.fullscreen then GLFW.glfwGetPrimaryMonitor() else NULL, NULL)

    if windowPtr == NULL then throw WindowCreationException("Failed to create GLFW window")

    val windowId = WindowId(windowIdGenerator.getAndIncrement())

    try
      val window = new GLFWWindow(windowId, windowPtr, config, this)
      activeWindows.put(windowId, window)
      setupWindowPosition(windowPtr, config)
      GLFW.glfwShowWindow(windowPtr)
      window
    catch
      case e: Exception =>
        GLFW.glfwDestroyWindow(windowPtr)
        throw WindowCreationException(s"Failed to initialize window with ID $windowId", e)

  override def destroyWindow(window: Window): Try[Unit] = Try:
    window match
      case glfwWindow: GLFWWindow =>
        activeWindows.remove(glfwWindow.id)
        glfwWindow.destroy()
      case _ =>
        throw WindowOperationException("Window is not a GLFW window")

  override def pollEvents(): Try[List[WindowEvent]] = Try:
    if !initialized.get() then throw WindowSystemNotInitializedException()

    try
      GLFW.glfwPollEvents()
      val allEvents = activeWindows.values.flatMap(_.pollEvents()).toList
      allEvents
    catch
      case e: Exception =>
        throw WindowOperationException("Failed to poll events", e)

  override def getActiveWindows(): List[Window] =
    activeWindows.values.toList

  override def findWindow(id: WindowId): Option[Window] =
    activeWindows.get(id)

  override def isInitialized: Boolean = initialized.get()

  // Internal method to remove window from tracking (called by GLFWWindow)
  private[platform] def unregisterWindow(windowId: WindowId): Unit =
    activeWindows.remove(windowId)

  private def applyWindowHints(config: WindowConfig): Unit =
    // Core window hints
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)
    GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, if config.resizable then GLFW.GLFW_TRUE else GLFW.GLFW_FALSE)
    GLFW.glfwWindowHint(GLFW.GLFW_DECORATED, if config.decorated then GLFW.GLFW_TRUE else GLFW.GLFW_FALSE)
    GLFW.glfwWindowHint(GLFW.GLFW_VISIBLE, GLFW.GLFW_FALSE) // We'll show it manually

    // MSAA samples
    if config.samples > 1 then GLFW.glfwWindowHint(GLFW.GLFW_SAMPLES, config.samples)

    // Platform-specific hints
    val osName = System.getProperty("os.name").toLowerCase
    if osName.contains("mac") then GLFW.glfwWindowHint(GLFW.GLFW_COCOA_GRAPHICS_SWITCHING, GLFW.GLFW_TRUE)
    else if osName.contains("win") then GLFW.glfwWindowHint(GLFW.GLFW_SCALE_TO_MONITOR, GLFW.GLFW_TRUE)

  private def setupWindowPosition(windowPtr: Long, config: WindowConfig): Unit =
    config.position match
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
