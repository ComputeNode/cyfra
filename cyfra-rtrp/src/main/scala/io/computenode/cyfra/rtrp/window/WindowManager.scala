package io.computenode.cyfra.rtrp.window

import io.computenode.cyfra.rtrp.window.core.*
import io.computenode.cyfra.rtrp.window.platform.GLFWWindowSystem
import io.computenode.cyfra.rtrp.surface.SurfaceManager
import io.computenode.cyfra.rtrp.surface.core.SurfaceConfig
import io.computenode.cyfra.rtrp.surface.core.{RenderSurface, SurfaceEvent}
import io.computenode.cyfra.vulkan.VulkanContext
import scala.util.{Try, Success, Failure}

class WindowManager:
  private var windowSystem: Option[WindowSystem] = None
  private var eventHandlers: Map[Class[? <: WindowEvent], WindowEvent => Unit] = Map.empty
  private var surfaceManager: Option[SurfaceManager] = None
  private var vulkanContext: Option[VulkanContext] = None

  // Initialize the window manager with GLFW backend only
  def initialize(): Try[Unit] = {
    if windowSystem.isDefined then return Failure(new IllegalStateException("WindowManager already initialized"))

    val glfwSystem = new GLFWWindowSystem()
    glfwSystem.initialize().map { _ =>
      windowSystem = Some(glfwSystem)
    }
  }

  // Initialize with Vulkan surface support
  def initializeWithVulkan(vkContext: VulkanContext): Try[Unit] =
    initialize().map { _ =>
      vulkanContext = Some(vkContext)
      surfaceManager = Some(new SurfaceManager(vkContext))

      setupDefaultSurfaceEventHandlers()
    }

  // Create a window with default configuration
  def createWindow(): Try[Window] =
    createWindow(WindowConfig())

  // Create a window with custom configuration
  def createWindow(config: WindowConfig): Try[Window] =
    windowSystem match {
      case Some(system) => system.createWindow(config)
      case None         => Failure(new IllegalStateException("WindowManager not initialized"))
    }

  // Create a window with builder-style configuration
  def createWindow(configure: WindowConfig => WindowConfig): Try[Window] = {
    val config = configure(WindowConfig())
    createWindow(config)
  }

  // Create a window with automatic surface creation
  def createWindowWithSurface(
    windowConfig: WindowConfig = WindowConfig(),
    surfaceConfig: SurfaceConfig = SurfaceConfig.default,
  ): Try[(Window, RenderSurface)] =

    surfaceManager match {
      case Some(surfMgr) =>
        for {
          window <- createWindow(windowConfig)
          surface <- surfMgr.createSurface(window, surfaceConfig)
        } yield (window, surface)

      case None =>
        Failure(new IllegalStateException("Surface manager not initialized. Call initializeWithVulkan() first."))
    }

  // Create multiple windows with surfaces (All-or-nothing approach for now)
  def createWindowsWithSurfaces(configs: List[(WindowConfig, SurfaceConfig)]): Try[List[(Window, RenderSurface)]] = {
    val results = configs.map { case (winConfig, surfConfig) =>
      createWindowWithSurface(winConfig, surfConfig)
    }

    val failures = results.collect { case Failure(ex) => ex }
    if failures.nonEmpty then {
      results.collect { case Success((window, surface)) =>
        surfaceManager.foreach(_.destroySurface(window.id))
        windowSystem.foreach(_.destroyWindow(window))
      }
      Failure(new RuntimeException(s"Failed to create ${failures.size} window-surface pairs"))
    } else Success(results.collect { case Success(pair) => pair })
  }

  def destroyWindow(window: Window): Try[Unit] =
    for {
      _ <- surfaceManager.map(_.destroySurface(window.id)).getOrElse(Success(()))
      _ <- windowSystem.map(_.destroyWindow(window)).getOrElse(Success(()))
    } yield ()

  def getSurfaceManager(): Option[SurfaceManager] = surfaceManager

  def getVulkanContext(): Option[VulkanContext] = vulkanContext

  def pollAndDispatchEvents(): Try[Unit] =
    windowSystem match
      case Some(system) =>
        system.pollEvents().map { events =>
          events.foreach { event =>
            // Dispatch to window event handlers
            dispatchEvent(event)

            // Also forward to surface manager if available
            surfaceManager.foreach(_.handleWindowEvent(event))
          }
        }
      case None =>
        Failure(new IllegalStateException("WindowManager not initialized"))

  def onEvent[T <: WindowEvent](eventClass: Class[T])(handler: T => Unit): Unit =
    eventHandlers = eventHandlers + (eventClass -> handler.asInstanceOf[WindowEvent => Unit])

  def onWindowClose(handler: WindowEvent.CloseRequested => Unit): Unit =
    onEvent(classOf[WindowEvent.CloseRequested])(handler)

  def onWindowResize(handler: WindowEvent.Resized => Unit): Unit =
    onEvent(classOf[WindowEvent.Resized])(handler)

  def onKeyPress(handler: InputEvent.KeyPressed => Unit): Unit =
    onEvent(classOf[InputEvent.KeyPressed])(handler)

  def onMouseClick(handler: InputEvent.MousePressed => Unit): Unit =
    onEvent(classOf[InputEvent.MousePressed])(handler)

  def onSurfaceCreated(handler: SurfaceEvent.SurfaceCreated => Unit): Unit =
    surfaceManager.foreach(_.onSurfaceCreated(handler))

  def onSurfaceDestroyed(handler: SurfaceEvent.SurfaceDestroyed => Unit): Unit =
    surfaceManager.foreach(_.onSurfaceDestroyed(handler))

  def onSurfaceLost(handler: SurfaceEvent.SurfaceLost => Unit): Unit =
    surfaceManager.foreach(_.onSurfaceLost(handler))

  def getActiveWindows(): List[Window] =
    windowSystem.map(_.getActiveWindows()).getOrElse(List.empty)

  def findWindow(id: WindowId): Option[Window] =
    windowSystem.flatMap(_.findWindow(id))

  def isInitialized: Boolean = windowSystem.isDefined

  def hasVulkanSupport: Boolean = surfaceManager.isDefined

  def shutdown(): Try[Unit] = {
    val results = List(surfaceManager.map(_.shutdown()).getOrElse(Success(())), windowSystem.map(_.shutdown()).getOrElse(Success(())))

    surfaceManager = None
    windowSystem = None
    vulkanContext = None
    eventHandlers = Map.empty

    results.find(_.isFailure).getOrElse(Success(()))
  }

  private def dispatchEvent(event: WindowEvent): Unit =
    eventHandlers.get(event.getClass).foreach(_(event))

  private def setupDefaultSurfaceEventHandlers(): Unit =
    surfaceManager.foreach { manager =>
      manager.onSurfaceCreated { event =>
        println(s"Surface ${event.surfaceId} created for window ${event.windowId}")
      }

      manager.onSurfaceDestroyed { event =>
        println(s"Surface ${event.surfaceId} destroyed for window ${event.windowId}")
      }

      manager.onSurfaceLost { event =>
        println(s"Surface ${event.surfaceId} lost for window ${event.windowId}: ${event.error}")
        // Attempt to recreate the surface
        manager.recreateSurface(event.windowId, "Surface lost")
      }
    }

// Companion object with factory methods
object WindowManager {

  def create(): Try[WindowManager] =
    val manager = new WindowManager()
    manager.initialize().map(_ => manager)

  // Create and initialize a WindowManager with Vulkan support
  def createWithVulkan(vulkanContext: VulkanContext): Try[WindowManager] = {
    val manager = new WindowManager()
    manager.initializeWithVulkan(vulkanContext).map(_ => manager)
  }

  // Create a WindowManager with automatic resource management
  def withManager[T](action: WindowManager => Try[T]): Try[T] =
    create().flatMap { manager =>
      try
        action(manager)
      finally
        manager.shutdown()
    }

  // Create a WindowManager with Vulkan and automatic resource management
  def withVulkanManager[T](vulkanContext: VulkanContext)(action: WindowManager => Try[T]): Try[T] =
    createWithVulkan(vulkanContext).flatMap { manager =>
      try
        action(manager)
      finally
        manager.shutdown()
    }
}
