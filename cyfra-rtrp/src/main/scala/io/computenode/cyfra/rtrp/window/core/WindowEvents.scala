package io.computenode.cyfra.rtrp.window.core

// Base trait for all window events
sealed trait WindowEvent:
  def windowId: WindowId

// Window lifecycle events
object WindowEvent:
  case class Created(windowId: WindowId) extends WindowEvent
  case class Destroyed(windowId: WindowId) extends WindowEvent
  case class CloseRequested(windowId: WindowId) extends WindowEvent
  case class Resized(windowId: WindowId, width: Int, height: Int) extends WindowEvent
  case class Moved(windowId: WindowId, x: Int, y: Int) extends WindowEvent
  case class FocusChanged(windowId: WindowId, focused: Boolean) extends WindowEvent
  case class VisibilityChanged(windowId: WindowId, visible: Boolean) extends WindowEvent
  case class Minimized(windowId: WindowId) extends WindowEvent
  case class Maximized(windowId: WindowId) extends WindowEvent
  case class Restored(windowId: WindowId) extends WindowEvent

// Input events
sealed trait InputEvent extends WindowEvent

object InputEvent:
  case class KeyPressed(windowId: WindowId, key: Key, modifiers: KeyModifiers) extends InputEvent
  case class KeyReleased(windowId: WindowId, key: Key, modifiers: KeyModifiers) extends InputEvent
  case class KeyRepeated(windowId: WindowId, key: Key, modifiers: KeyModifiers) extends InputEvent
  case class CharacterInput(windowId: WindowId, codepoint: Int) extends InputEvent

  case class MousePressed(windowId: WindowId, button: MouseButton, x: Double, y: Double, modifiers: KeyModifiers) extends InputEvent
  case class MouseReleased(windowId: WindowId, button: MouseButton, x: Double, y: Double, modifiers: KeyModifiers) extends InputEvent
  case class MouseMoved(windowId: WindowId, x: Double, y: Double) extends InputEvent
  case class MouseScrolled(windowId: WindowId, xOffset: Double, yOffset: Double) extends InputEvent
  case class MouseEntered(windowId: WindowId) extends InputEvent
  case class MouseExited(windowId: WindowId) extends InputEvent

case class Key(code: Int)
case class MouseButton(code: Int)

case class KeyModifiers(shift: Boolean = false, ctrl: Boolean = false, alt: Boolean = false, `super`: Boolean = false)
