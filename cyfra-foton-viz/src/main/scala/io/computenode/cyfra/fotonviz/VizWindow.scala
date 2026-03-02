package io.computenode.cyfra.fotonviz

import org.lwjgl.BufferUtils
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil.NULL

import java.nio.{ByteBuffer, DoubleBuffer}

/** GLFW-based window for interactive visualization.
  *
  * @param width
  *   Window width in pixels
  * @param height
  *   Window height in pixels
  * @param title
  *   Window title
  */
class VizWindow(val width: Int, val height: Int, title: String) extends AutoCloseable:
  private var windowHandle: Long = NULL

  private var _mouseX: Double = 0.0
  private var _mouseY: Double = 0.0
  private var _mousePressed: Boolean = false

  def mouseX: Float = _mouseX.toFloat
  def mouseY: Float = _mouseY.toFloat
  def mousePressed: Boolean = _mousePressed

  private val xBuffer: DoubleBuffer = BufferUtils.createDoubleBuffer(1)
  private val yBuffer: DoubleBuffer = BufferUtils.createDoubleBuffer(1)

  /** Update mouse position by polling (more reliable than callbacks) */
  def updateMousePosition(): Unit =
    xBuffer.rewind()
    yBuffer.rewind()
    glfwGetCursorPos(windowHandle, xBuffer, yBuffer)
    _mouseX = xBuffer.get(0)
    _mouseY = yBuffer.get(0)

  /** Normalized mouse coordinates in range [-1, 1] */
  def normalizedMouseX: Float = (_mouseX.toFloat / width.toFloat) * 2.0f - 1.0f
  def normalizedMouseY: Float = 1.0f - (_mouseY.toFloat / height.toFloat) * 2.0f

  /** Initialize the window and OpenGL context */
  def init(): Unit =
    GLFWErrorCallback.createPrint(System.err).set() 

    if !glfwInit() then throw new IllegalStateException("Unable to initialize GLFW")

    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_COMPAT_PROFILE)

    windowHandle = glfwCreateWindow(width, height, title, NULL, NULL)
    if windowHandle == NULL then throw new RuntimeException("Failed to create the GLFW window")

    glfwSetCursorPosCallback(windowHandle, (_, xpos, ypos) =>
      _mouseX = xpos
      _mouseY = ypos,
    )

    glfwSetMouseButtonCallback(windowHandle, (_, button, action, _) =>
      if button == GLFW_MOUSE_BUTTON_LEFT then _mousePressed = action == GLFW_PRESS || action == GLFW_REPEAT,
    )

    glfwSetKeyCallback(windowHandle, (window, key, _, action, _) =>
      if key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE then glfwSetWindowShouldClose(window, true),
    )

    val stack = MemoryStack.stackPush()
    try
      val pWidth = stack.mallocInt(1)
      val pHeight = stack.mallocInt(1)
      glfwGetWindowSize(windowHandle, pWidth, pHeight)

      val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor())
      glfwSetWindowPos(
        windowHandle,
        (vidmode.width() - pWidth.get(0)) / 2,
        (vidmode.height() - pHeight.get(0)) / 2,
      )
    finally stack.close()

    glfwMakeContextCurrent(windowHandle)
    glfwSwapInterval(1)
    glfwShowWindow(windowHandle)

    GL.createCapabilities()
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, width, height, 0, -1, 1)
    glMatrixMode(GL_MODELVIEW)

  /** Check if the window should close */
  def shouldClose: Boolean = glfwWindowShouldClose(windowHandle)

  /** Poll for window events */
  def pollEvents(): Unit = glfwPollEvents()

  /** Swap the front and back buffers */
  def swapBuffers(): Unit = glfwSwapBuffers(windowHandle)

  /** Render pixel data from a ByteBuffer (RGBA format, 4 bytes per pixel) */
  def renderPixels(pixels: ByteBuffer): Unit =
    glClear(GL_COLOR_BUFFER_BIT)

    pixels.rewind()
    // Use glWindowPos2i instead of glRasterPos2i to avoid clipping issues
    // Position at bottom-left (0, 0) in window coordinates
    org.lwjgl.opengl.GL14.glWindowPos2i(0, 0)
    glDrawPixels(width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels)

    swapBuffers()

  /** Render pixel data from a float array (RGBA format, 4 floats per pixel) */
  def renderFloatPixels(pixels: Array[Float]): Unit =
    glClear(GL_COLOR_BUFFER_BIT)

    glRasterPos2i(0, 0)
    glDrawPixels(width, height, GL_RGBA, GL_FLOAT, pixels)

    swapBuffers()

  override def close(): Unit =
    if windowHandle != NULL then
      glfwFreeCallbacks(windowHandle)
      glfwDestroyWindow(windowHandle)
    glfwTerminate()
    Option(glfwSetErrorCallback(null)).foreach(_.free())

object VizWindow:
  /** Create and initialize a window */
  def create(width: Int, height: Int, title: String): VizWindow =
    val window = new VizWindow(width, height, title)
    window.init()
    window
