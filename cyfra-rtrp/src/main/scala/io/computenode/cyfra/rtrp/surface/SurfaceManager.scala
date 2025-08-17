package io.computenode.cyfra.rtrp.surface

import io.computenode.cyfra.rtrp.surface.core.*
import io.computenode.cyfra.rtrp.surface.vulkan.VulkanSurfaceFactory
import io.computenode.cyfra.rtrp.window.core.*
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.command.Queue
import scala.collection.mutable
import scala.util.*
import io.computenode.cyfra.utility.Logger.logger

// High-level surface manager that integrates with the window system
class SurfaceManager(vulkanContext: VulkanContext):

  private val surfaceFactory = new VulkanSurfaceFactory(vulkanContext)
  private val activeSurfaces = mutable.Map[WindowId, Surface]()
  private val surfaceConfigs = mutable.Map[WindowId, SurfaceConfig]()
  private val eventHandlers = mutable.Map[Class[? <: SurfaceEvent], SurfaceEvent => Unit]()

  def initializePresentQueue(surface: Surface): Try[Queue] = Try {
    val device = vulkanContext.device
    val presentQueueFamily = device.findPresentQueueFamily(surface.nativeHandle)

    if (presentQueueFamily == device.graphicsQueueFamily) {
      vulkanContext.graphicsQueue
    } else {
      new Queue(presentQueueFamily, 0, device)
    }
  }

  // Create a surface for a window.
  def createSurface(window: Window, config: SurfaceConfig = SurfaceConfig.default): Try[Surface] =
    if activeSurfaces.contains(window.id) then return Failure(new IllegalStateException(s"Surface already exists for window ${window.id}"))

    surfaceFactory
      .createSurface(window, config)
      .map: surface =>
        activeSurfaces(window.id) = surface
        surfaceConfigs(window.id) = config

        surface
          .getCapabilities()
          .foreach: capabilities =>
            fireEvent(SurfaceEvent.SurfaceCreated(window.id, surface.id, capabilities))

        surface

  def createSurfaces(windows: List[Window], config: SurfaceConfig = SurfaceConfig.default): Try[List[Surface]] =
    val results = windows.map(createSurface(_, config))

    val failures = results.collect { case Failure(ex) => ex }
    if failures.nonEmpty then
      results.collect { case Success(surface) => destroySurface(surface.windowId) }
      Failure(new RuntimeException(s"Failed to create ${failures.size} surfaces"))
    else Success(results.collect { case Success(surface) => surface })

  def getSurface(windowId: WindowId): Option[Surface] =
    activeSurfaces.get(windowId)

  def getActiveSurfaces(): Map[WindowId, Surface] = activeSurfaces.toMap

  def getSurfaceConfig(windowId: WindowId): Option[SurfaceConfig] =
    surfaceConfigs.get(windowId)

  def updateSurfaceConfig(windowId: WindowId, newConfig: SurfaceConfig): Try[Unit] =
    getSurface(windowId) match
      case Some(surface) =>
        surfaceConfigs(windowId) = newConfig
        // Note: Actual surface reconfiguration would happen in swapchain recreation
        Success(())
      case None =>
        Failure(new IllegalArgumentException(s"No surface found for window $windowId"))

  def destroySurface(windowId: WindowId): Try[Unit] =
    activeSurfaces.remove(windowId) match
      case Some(surface) =>
        surfaceConfigs.remove(windowId)

        val result = surface.destroy()

        fireEvent(SurfaceEvent.SurfaceDestroyed(windowId, surface.id))

        result
      case None =>
        Success(())

  def handleWindowEvent(event: WindowEvent): Try[Unit] = Try:
    event match
      case WindowEvent.Resized(windowId, width, height) =>
        val result = for
          surface <- activeSurfaces.get(windowId).toRight(new NoSuchElementException(s"No surface for window $windowId")).toTry
          _ <- surface.resize(width, height)
          newCapabilities <- surface.getCapabilities()
        yield fireEvent(
          SurfaceEvent.SurfaceCapabilitiesChanged(
            windowId,
            surface.id,
            newCapabilities,
            newCapabilities, // TODO: track old capabilities
          ),
        )

        result.recover:
          case ex => logger.error(s"Failed to resize surface for window $windowId: ${ex.getMessage}")

      case WindowEvent.CloseRequested(windowId) =>
        destroySurface(windowId).recover:
          case ex =>
            logger.error(s"Failed to destroy surface for closing window $windowId: ${ex.getMessage}")

      case WindowEvent.Destroyed(windowId) =>
        destroySurface(windowId).recover:
          case ex =>
            logger.error(s"Failed to destroy surface for destroyed window $windowId: ${ex.getMessage}")

      case _ =>
      // Ignore other events

  // Register an event handler for surface events
  def onSurfaceEvent[T <: SurfaceEvent](eventClass: Class[T])(handler: T => Unit): Unit =
    eventHandlers(eventClass) = handler.asInstanceOf[SurfaceEvent => Unit]

  // Convenience methods for common surface events

  def onSurfaceCreated(handler: SurfaceEvent.SurfaceCreated => Unit): Unit =
    onSurfaceEvent(classOf[SurfaceEvent.SurfaceCreated])(handler)

  def onSurfaceDestroyed(handler: SurfaceEvent.SurfaceDestroyed => Unit): Unit =
    onSurfaceEvent(classOf[SurfaceEvent.SurfaceDestroyed])(handler)

  def onSurfaceLost(handler: SurfaceEvent.SurfaceLost => Unit): Unit =
    onSurfaceEvent(classOf[SurfaceEvent.SurfaceLost])(handler)

  // Recreate surface (useful for device lost scenarios like GPU driver crash, External monitor disconnect, etc.)
  def recreateSurface(windowId: WindowId, reason: String = "Manual recreation"): Try[Unit] =
    getSurface(windowId) match
      case Some(surface) =>
        surface
          .recreate()
          .map: _ =>
            surface
              .getCapabilities()
              .foreach: newCapabilities =>
                fireEvent(SurfaceEvent.SurfaceRecreated(windowId, surface.id, reason, newCapabilities))

      case None =>
        Failure(new IllegalArgumentException(s"No surface found for window $windowId"))

  def shutdown(): Try[Unit] = Try:
    val failures = activeSurfaces.keys.map(destroySurface).collect { case Failure(ex) =>
      ex
    }

    activeSurfaces.clear()
    surfaceConfigs.clear()
    eventHandlers.clear()

    if failures.nonEmpty then throw new RuntimeException(s"Failed to destroy ${failures.size} surfaces")

  // Get statistics about managed surfaces.
  def getStatistics(): SurfaceManagerStatistics =
    SurfaceManagerStatistics(
      totalSurfaces = activeSurfaces.size,
      validSurfaces = activeSurfaces.values.count(_.isValid),
      invalidSurfaces = activeSurfaces.values.count(!_.isValid),
      windowIds = activeSurfaces.keys.toList,
    )

  private def fireEvent(event: SurfaceEvent): Unit =
    eventHandlers.get(event.getClass).foreach(_(event))

// Statistics about the surface manager
case class SurfaceManagerStatistics(totalSurfaces: Int, validSurfaces: Int, invalidSurfaces: Int, windowIds: List[WindowId]):
  override def toString: String =
    s"SurfaceManager(total: $totalSurfaces, valid: $validSurfaces, invalid: $invalidSurfaces)"
