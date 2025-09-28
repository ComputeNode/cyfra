package io.computenode.cyfra.rtrp.surface.core

import io.computenode.cyfra.rtrp.window.core.*
import scala.util.Try

// Unique id for surfaces
case class SurfaceId(value: Long) extends AnyVal

// Render surface abstraction
trait Surface:
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

  def recreate(): Try[Unit] =
    for
      (width, height) <- currentSize
      _ <- resize(width, height)
    yield ()
