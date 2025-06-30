package io.computenode.cyfra.window.platform

import io.computenode.cyfra.window.core.*
import org.lwjgl.glfw.GLFW
import org.lwjgl.system.MemoryStack
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicBoolean

// GLFW implementation of the Window trait.
class GLFWWindow(val id: WindowId, val nativeHandle: Long, initialConfig: WindowConfig, windowSystem: GLFWWindowSystem) extends Window {

  private val destroyed = new AtomicBoolean(false)
  private val eventBuffer = ListBuffer[WindowEvent]()
  private var currentProperties = createInitialProperties(initialConfig)

  setupCallbacks()

  override def properties: WindowProperties = currentProperties

  override def show(): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwShowWindow(nativeHandle)
    currentProperties = currentProperties.copy(visible = true)
  }

  override def hide(): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwHideWindow(nativeHandle)
    currentProperties = currentProperties.copy(visible = false)
  }

  override def close(): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwSetWindowShouldClose(nativeHandle, true)
  }

  override def focus(): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwFocusWindow(nativeHandle)
  }

  override def minimize(): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwIconifyWindow(nativeHandle)
  }

  override def maximize(): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwMaximizeWindow(nativeHandle)
  }

  override def restore(): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwRestoreWindow(nativeHandle)
  }

  override def setTitle(title: String): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwSetWindowTitle(nativeHandle, title)
    currentProperties = currentProperties.copy(title = title)
  }

  override def setSize(width: Int, height: Int): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwSetWindowSize(nativeHandle, width, height)
  }

  override def setPosition(x: Int, y: Int): Try[Unit] = Try {
    checkNotDestroyed()
    GLFW.glfwSetWindowPos(nativeHandle, x, y)
  }

  override def shouldClose: Boolean =
    if destroyed.get() then true
    else GLFW.glfwWindowShouldClose(nativeHandle)

  override def isVisible: Boolean = currentProperties.visible
  override def isFocused: Boolean = currentProperties.focused
  override def isMinimized: Boolean = currentProperties.minimized
  override def isMaximized: Boolean = currentProperties.maximized

  // Internal methods
  private[platform] def pollEvents(): List[WindowEvent] = {
    val events = eventBuffer.toList
    eventBuffer.clear()
    events
  }

  private[platform] def destroy(): Unit =
    if !destroyed.getAndSet(true) then {
      GLFW.glfwDestroyWindow(nativeHandle)
      windowSystem.unregisterWindow(id)
    }

  private def checkNotDestroyed(): Unit =
    if destroyed.get() then throw new IllegalStateException("Window has been destroyed")

  private def createInitialProperties(config: WindowConfig): WindowProperties =
    WindowProperties(
      width = config.width,
      height = config.height,
      title = config.title,
      visible = false, // will be set to true when shown
      focused = false,
      minimized = false,
      maximized = false,
    )

  private def setupCallbacks(): Unit = {
    GLFW.glfwSetWindowCloseCallback(nativeHandle, (window: Long) => eventBuffer += WindowEvent.CloseRequested(id))

    GLFW.glfwSetWindowSizeCallback(
      nativeHandle,
      (window: Long, width: Int, height: Int) => {
        currentProperties = currentProperties.copy(width = width, height = height)
        eventBuffer += WindowEvent.Resized(id, width, height)
      },
    )

    GLFW.glfwSetWindowPosCallback(nativeHandle, (window: Long, x: Int, y: Int) => eventBuffer += WindowEvent.Moved(id, x, y))

    GLFW.glfwSetWindowFocusCallback(
      nativeHandle,
      (window: Long, focused: Boolean) => {
        currentProperties = currentProperties.copy(focused = focused)
        eventBuffer += WindowEvent.FocusChanged(id, focused)
      },
    )

    GLFW.glfwSetWindowIconifyCallback(
      nativeHandle,
      (window: Long, iconified: Boolean) => {
        currentProperties = currentProperties.copy(minimized = iconified)
        if iconified then eventBuffer += WindowEvent.Minimized(id)
        else eventBuffer += WindowEvent.Restored(id)
      },
    )

    GLFW.glfwSetWindowMaximizeCallback(
      nativeHandle,
      (window: Long, maximized: Boolean) => {
        currentProperties = currentProperties.copy(maximized = maximized)
        if maximized then eventBuffer += WindowEvent.Maximized(id)
        else eventBuffer += WindowEvent.Restored(id)
      },
    )

    // Key callbacks
    GLFW.glfwSetKeyCallback(
      nativeHandle,
      (window: Long, key: Int, scancode: Int, action: Int, mods: Int) => {
        val keyModifiers = createKeyModifiers(mods)
        val keyObj = Key(key)

        action match {
          case GLFW.GLFW_PRESS =>
            eventBuffer += InputEvent.KeyPressed(id, keyObj, keyModifiers)
          case GLFW.GLFW_RELEASE =>
            eventBuffer += InputEvent.KeyReleased(id, keyObj, keyModifiers)
          case GLFW.GLFW_REPEAT =>
            eventBuffer += InputEvent.KeyRepeated(id, keyObj, keyModifiers)
        }
      },
    )

    // Character input callback
    GLFW.glfwSetCharCallback(nativeHandle, (window: Long, codepoint: Int) => eventBuffer += InputEvent.CharacterInput(id, codepoint))

    // Mouse button callbacks
    GLFW.glfwSetMouseButtonCallback(
      nativeHandle,
      (window: Long, button: Int, action: Int, mods: Int) => {
        val stack = MemoryStack.stackPush()
        try {
          val xPos = stack.mallocDouble(1)
          val yPos = stack.mallocDouble(1)
          GLFW.glfwGetCursorPos(window, xPos, yPos)

          val x = xPos.get()
          val y = yPos.get()
          val keyModifiers = createKeyModifiers(mods)
          val mouseButton = MouseButton(button)

          action match {
            case GLFW.GLFW_PRESS =>
              eventBuffer += InputEvent.MousePressed(id, mouseButton, x, y, keyModifiers)
            case GLFW.GLFW_RELEASE =>
              eventBuffer += InputEvent.MouseReleased(id, mouseButton, x, y, keyModifiers)
          }
        } finally stack.pop()
      },
    )

    // Cursor position callback
    GLFW.glfwSetCursorPosCallback(nativeHandle, (window: Long, xpos: Double, ypos: Double) => eventBuffer += InputEvent.MouseMoved(id, xpos, ypos))

    // Cursor enter/leave callback
    GLFW.glfwSetCursorEnterCallback(
      nativeHandle,
      (window: Long, entered: Boolean) =>
        if entered then eventBuffer += InputEvent.MouseEntered(id)
        else eventBuffer += InputEvent.MouseExited(id),
    )

    // Scroll callback
    GLFW.glfwSetScrollCallback(
      nativeHandle,
      (window: Long, xoffset: Double, yoffset: Double) => eventBuffer += InputEvent.MouseScrolled(id, xoffset, yoffset),
    )
  }

  private def createKeyModifiers(mods: Int): KeyModifiers =
    KeyModifiers(
      shift = (mods & GLFW.GLFW_MOD_SHIFT) != 0,
      ctrl = (mods & GLFW.GLFW_MOD_CONTROL) != 0,
      alt = (mods & GLFW.GLFW_MOD_ALT) != 0,
      `super` = (mods & GLFW.GLFW_MOD_SUPER) != 0,
    )
}
