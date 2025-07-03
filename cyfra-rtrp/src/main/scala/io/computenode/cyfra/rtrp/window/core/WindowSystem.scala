package io.computenode.cyfra.rtrp.window.core

import scala.util.Try

// Main interface for window system operations
trait WindowSystem:

  def initialize(): Try[Unit]
  def shutdown(): Try[Unit]
  def createWindow(config: WindowConfig): Try[Window]
  def destroyWindow(window: Window): Try[Unit]
  def pollEvents(): Try[List[WindowEvent]]
  def getActiveWindows(): List[Window]
  def findWindow(id: WindowId): Option[Window]
  def isInitialized: Boolean
