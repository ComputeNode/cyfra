package io.computenode.cyfra.rtrp.surface.core

import io.computenode.cyfra.rtrp.window.core.*

// Surface-specific events that extend window events
sealed trait SurfaceEvent extends WindowEvent

object SurfaceEvent:
  case class SurfaceCreated(windowId: WindowId, surfaceId: SurfaceId, capabilities: SurfaceCapabilities) extends SurfaceEvent

  case class SurfaceDestroyed(windowId: WindowId, surfaceId: SurfaceId) extends SurfaceEvent

  case class SurfaceRecreated(windowId: WindowId, surfaceId: SurfaceId, reason: String, newCapabilities: SurfaceCapabilities) extends SurfaceEvent

  case class SurfaceCapabilitiesChanged(
    windowId: WindowId,
    surfaceId: SurfaceId,
    oldCapabilities: SurfaceCapabilities,
    newCapabilities: SurfaceCapabilities,
  ) extends SurfaceEvent

  case class SurfaceLost(windowId: WindowId, surfaceId: SurfaceId, error: String) extends SurfaceEvent

  case class SurfaceFormatChanged(windowId: WindowId, surfaceId: SurfaceId, oldFormat: SurfaceFormat, newFormat: SurfaceFormat) extends SurfaceEvent
