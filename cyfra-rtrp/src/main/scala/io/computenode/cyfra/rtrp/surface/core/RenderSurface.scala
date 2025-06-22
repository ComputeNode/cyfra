package io.computenode.cyfra.rtrp.surface.core

import io.computenode.cyfra.rtrp.window.core.{Window, WindowId}
import scala.util.Try

// Render surface abstraction
trait RenderSurface {
  def id: SurfaceId
  def windowId: WindowId
  def nativeHandle: Long
  def isValid: Boolean
  
  // Surface operations
  def resize(width: Int, height: Int): Try[Unit]
  def getCapabilities(): Try[SurfaceCapabilities]
  def destroy(): Try[Unit]
  
  // Surface properties
  def currentSize: Try[(Int, Int)]
  def isDestroyed: Boolean = !isValid

  def recreate(): Try[Unit] = {
    for {
      (width, height) <- currentSize
      _ <- resize(width, height)
    } yield ()
  }
}