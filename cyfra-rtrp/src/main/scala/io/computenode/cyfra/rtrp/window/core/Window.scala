package io.computenode.cyfra.window.core

import scala.util.Try

// Unique identifier for windows
case class WindowId(value: Long) extends AnyVal

// Platform-agnostic window interface
trait Window {
  def id: WindowId
  def properties: WindowProperties
  def nativeHandle: Long // Platform-specific handle
  
  // Window operations
  def show(): Try[Unit]
  def hide(): Try[Unit]
  def close(): Try[Unit]
  def focus(): Try[Unit]
  def minimize(): Try[Unit]
  def maximize(): Try[Unit]
  def restore(): Try[Unit]
  
  // Property changes
  def setTitle(title: String): Try[Unit]
  def setSize(width: Int, height: Int): Try[Unit]
  def setPosition(x: Int, y: Int): Try[Unit]
  
  // Queries
  def shouldClose: Boolean
  def isVisible: Boolean
  def isFocused: Boolean
  def isMinimized: Boolean
  def isMaximized: Boolean
}