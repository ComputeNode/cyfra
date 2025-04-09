package io.computenode.cyfra.window

/**
 * Platform-agnostic handle to a window instance.
 */
trait WindowHandle {
  /**
   * Returns the native window pointer.
   * For GLFW, this is the GLFWwindow pointer as a long value.
   */
  def nativePtr: Long
}

/**
 * Implementation of WindowHandle for GLFW windows.
 */
class GLFWWindowHandle(val nativePtr: Long) extends WindowHandle {
  // GLFW-specific window operations could be added here if needed
}

/**
 * Represents window-related events.
 */
sealed trait WindowEvent

/**
 * Common window events.
 */
object WindowEvent {
  /**
   * Window resize event.
   * 
   * @param width The new width of the window
   * @param height The new height of the window
   */
  case class Resize(width: Int, height: Int) extends WindowEvent
  
  /**
   * Key event.
   * 
   * @param keyCode The GLFW key code
   * @param action The action (press, release, repeat)
   * @param mods Modifier keys that were held
   */
  case class Key(keyCode: Int, action: Int, mods: Int) extends WindowEvent
  
  /**
   * Character input event (for text input).
   * 
   * @param codepoint Unicode code point of the character
   */
  case class CharInput(codepoint: Int) extends WindowEvent
  
  /**
   * Mouse movement event.
   * 
   * @param x The x coordinate
   * @param y The y coordinate
   */
  case class MouseMove(x: Double, y: Double) extends WindowEvent
  
  /**
   * Mouse button event.
   * 
   * @param button The button number
   * @param pressed True if pressed, false if released
   * @param x The x coordinate
   * @param y The y coordinate
   */
  case class MouseButton(button: Int, pressed: Boolean, x: Double, y: Double) extends WindowEvent
  
  /**
   * Scroll wheel event.
   * 
   * @param xOffset Horizontal scroll amount
   * @param yOffset Vertical scroll amount
   */
  case class Scroll(xOffset: Double, yOffset: Double) extends WindowEvent
  
  /**
   * Window close request event.
   */
  case object Close extends WindowEvent
}