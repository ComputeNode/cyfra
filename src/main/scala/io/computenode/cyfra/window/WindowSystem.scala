package io.computenode.cyfra.window

/**
 * Platform-agnostic interface for window management operations.
 */
trait WindowSystem {
  /**
   * Creates a new window with specified dimensions and title.
   *
   * @param width The width of the window in pixels
   * @param height The height of the window in pixels
   * @param title The window title
   * @return A handle to the created window
   */
  def createWindow(width: Int, height: Int, title: String): WindowHandle
  
  /**
   * Polls for and returns pending window events.
   *
   * @return List of window events that occurred since the last poll
   */
  def pollEvents(): List[WindowEvent]
  
  /**
   * Checks if a window should close.
   *
   * @param window The window handle to check
   * @return true if the window should close, false otherwise
   */
  def shouldWindowClose(window: WindowHandle): Boolean
  
  /**
   * Destroys a window and releases its resources.
   *
   * @param window The window handle to destroy
   */
  def destroyWindow(window: WindowHandle): Unit
}