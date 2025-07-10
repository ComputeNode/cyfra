package io.computenode.cyfra.rtrp.surface

import io.computenode.cyfra.rtrp.window.WindowManager
import io.computenode.cyfra.rtrp.window.WindowManager.*
import io.computenode.cyfra.rtrp.window.core.WindowConfig
import io.computenode.cyfra.rtrp.window.core.WindowPosition
import io.computenode.cyfra.rtrp.surface.core.*
import io.computenode.cyfra.vulkan.VulkanContext
import scala.util.*

// Complete example demonstrating the integrated window + surface system.
object SurfaceIntegrationExample:

  def main(args: Array[String]): Unit =
    println("=== Cyfra Surface Integration Example ===")

    val result = runFullExample()

    result match
      case Success(_) =>
        println("Surface integration example completed successfully!")
      case Failure(ex) =>
        println(s"Surface integration example failed: ${ex.getMessage}")
        ex.printStackTrace()

  private def runFullExample(): Try[Unit] = Try:
    println("=== Cyfra Surface Integration Example ===\n")

    // Initialize GLFW first
    import org.lwjgl.glfw.GLFW
    if !GLFW.glfwInit() then throw new RuntimeException("Failed to initialize GLFW")

    try
      // Now create VulkanContext with surface support
      val vulkanContext = VulkanContext.withSurfaceSupport()

      // Validate that the instance is properly created
      if vulkanContext.instance.get.address() == 0L then throw new RuntimeException("VulkanContext instance is null")

      WindowManager.withVulkanManager(vulkanContext): manager =>
        Try:
          setupEventHandlers(manager)
          println("Event handlers configured\n")

          val windowsAndSurfaces = createTestWindows(manager)
          println(s"Created ${windowsAndSurfaces.size} window-surface pairs\n")

          inspectSurfaceCapabilities(windowsAndSurfaces)

          runMainLoop(manager, windowsAndSurfaces)
          println("Main loop completed\n")

          windowsAndSurfaces.foreach { case (window, surface) =>
            testSurfaceRecreation(manager, surface)
          }

    finally GLFW.glfwTerminate()

  private def setupEventHandlers(manager: WindowManager): Unit =
    // Window event handlers
    manager.onWindowResize: event =>
      println(s"Window ${event.windowId} resized to ${event.width}x${event.height}")

    manager.onWindowClose: event =>
      println(s"Window ${event.windowId} close requested")

    manager.onKeyPress: event =>
      println(s"Key ${event.key.code} pressed in window ${event.windowId}")

    manager.onMouseClick: event =>
      println(s"Mouse button ${event.button.code} clicked at (${event.x.toInt}, ${event.y.toInt}) in window ${event.windowId}")

    // Surface event handlers
    manager.onSurfaceCreated: event =>
      println(s"Surface ${event.surfaceId} created for window ${event.windowId}")
      val caps = event.capabilities
      println(s"Formats: ${caps.supportedFormats.size}, Present modes: ${caps.supportedPresentModes.size}")

    manager.onSurfaceDestroyed: event =>
      println(s"Surface ${event.surfaceId} destroyed for window ${event.windowId}")

    manager.onSurfaceLost: event =>
      println(s"Surface ${event.surfaceId} lost for window ${event.windowId}: ${event.error}")

  private def createTestWindows(
    manager: WindowManager,
  ): List[(io.computenode.cyfra.rtrp.window.core.Window, io.computenode.cyfra.rtrp.surface.core.RenderSurface)] =
    val configs = List(
      // Main window - gaming configuration
      (WindowConfig(width = 1024, height = 768, title = "Main Window", position = Some(WindowPosition.Centered)), SurfaceConfig.gaming),

      // Secondary window - quality configuration
      (WindowConfig(width = 800, height = 600, title = "Secondary Window", position = Some(WindowPosition.Fixed(100, 100))), SurfaceConfig.quality),

      // Tool window - low latency configuration
      (WindowConfig(width = 400, height = 300, title = "Tool Window", position = Some(WindowPosition.Fixed(200, 200))), SurfaceConfig.lowLatency),
    )

    manager.createWindowsWithSurfaces(configs) match
      case Success(pairs) => pairs
      case Failure(ex)    =>
        println(s"Failed to create windows: ${ex.getMessage}")
        List.empty

  private def inspectSurfaceCapabilities(
    windowSurfacePairs: List[(io.computenode.cyfra.rtrp.window.core.Window, io.computenode.cyfra.rtrp.surface.core.RenderSurface)],
  ): Unit =
    windowSurfacePairs.foreach { case (window, surface) =>
      println(s"\n  Surface ${surface.id} (Window: ${window.properties.title}):")

      surface.getCapabilities() match
        case Success(caps) =>
          println(s"Current size: ${caps.currentExtent}")
          println(s"Size range: ${caps.minImageExtent} to ${caps.maxImageExtent}")
          println(s"Image count: ${caps.minImageCount} to ${caps.maxImageCount}")
          println(
            s"Formats (${caps.supportedFormats.size}): ${caps.supportedFormats.take(3).mkString(", ")}${if caps.supportedFormats.size > 3 then "..." else ""}",
          )
          println(s"Present modes (${caps.supportedPresentModes.size}): ${caps.supportedPresentModes.mkString(", ")}")
          println(s"Alpha support: ${caps.supportsAlpha}")
          println(s"Transform support: ${caps.supportsTransform}")

        case Failure(ex) =>
          println(s"Failed to get capabilities: ${ex.getMessage}")
    }

  private def runMainLoop(
    manager: WindowManager,
    windowSurfacePairs: List[(io.computenode.cyfra.rtrp.window.core.Window, io.computenode.cyfra.rtrp.surface.core.RenderSurface)],
  ): Unit =
    var frameCount = 0
    val maxFrames = 300 // 5 seconds at 60fps
    val windows = windowSurfacePairs.map(_._1)
    val surfaces = windowSurfacePairs.map(_._2)

    while frameCount < maxFrames && windows.exists(!_.shouldClose) do
      // Poll and handle events
      manager.pollAndDispatchEvents() match
        case Success(_)  => // Events handled successfully
        case Failure(ex) => println(s"Warning: Event polling failed: ${ex.getMessage}")

      // Simulate rendering work
      if frameCount % 60 == 0 then
        val seconds = frameCount / 60 + 1
        val validSurfaces = surfaces.count(_.isValid)
        val openWindows = windows.count(!_.shouldClose)
        println(s"${seconds}s: $openWindows windows open, $validSurfaces surfaces valid")

        // Print surface manager statistics
        manager
          .getSurfaceManager()
          .foreach: surfMgr =>
            val stats = surfMgr.getStatistics()
            println(s"Surface stats: $stats")

      frameCount += 1
      Thread.sleep(16) // ~60 FPS

    println("Main loop completed")

  private def testSurfaceRecreation(manager: WindowManager, surface: io.computenode.cyfra.rtrp.surface.core.RenderSurface): Unit =
    println(s"Testing recreation of surface ${surface.id}...")

    manager.getSurfaceManager() match
      case Some(surfMgr) =>
        surfMgr.recreateSurface(surface.windowId, "Test recreation") match
          case Success(_) =>
            println("Surface recreation successful")
          case Failure(ex) =>
            println(s"Surface recreation failed: ${ex.getMessage}")
      case None =>
        println("No surface manager available")
