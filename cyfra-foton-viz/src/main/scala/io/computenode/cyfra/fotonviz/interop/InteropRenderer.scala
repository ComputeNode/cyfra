package io.computenode.cyfra.fotonviz.interop

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.{glActiveTexture, GL_TEXTURE0}
import org.lwjgl.opengl.GL30.{GL_RGBA16F, GL_HALF_FLOAT}
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL21.GL_PIXEL_UNPACK_BUFFER
import org.lwjgl.opengl.GL30.*
import org.lwjgl.opengl.GL45.*
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.vulkan.{VkDevice, VkPhysicalDevice}

/** Renderer using Vulkan-OpenGL interop for zero-copy display. */
class InteropRenderer(val width: Int, val height: Int, title: String) extends AutoCloseable:

  private var windowHandle: Long = 0
  private var sharedBuffer: SharedBuffer = null
  private var shaderProgram: Int = 0
  private var vao: Int = 0
  private var texture: Int = 0

  private var _mouseX: Double = 0
  private var _mouseY: Double = 0

  init()

  private def init(): Unit =
    GLFWErrorCallback.createPrint(System.err).set()
    if !glfwInit() then throw new IllegalStateException("Unable to initialize GLFW")

    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 5)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)

    windowHandle = glfwCreateWindow(width, height, title, NULL, NULL)
    if windowHandle == NULL then throw new RuntimeException("Failed to create GLFW window")

    glfwMakeContextCurrent(windowHandle)
    GL.createCapabilities()
    glfwSwapInterval(0)

    if !VulkanGLInterop.checkGLSupport() then
      throw new RuntimeException("OpenGL does not support required interop extensions")

    shaderProgram = createShaderProgram()
    vao = glGenVertexArrays()
    glBindVertexArray(vao)

    texture = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, texture)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, width, height, 0, GL_RGBA, GL_HALF_FLOAT, NULL)

    glfwShowWindow(windowHandle)

  def initSharedBuffer(vkDevice: VkDevice, vkPhysicalDevice: VkPhysicalDevice): SharedBuffer =
    // 8 bytes per pixel: RGBA16F = 4 channels × 2 bytes (Float16)
    sharedBuffer = SharedBuffer(width * height * 8, vkDevice, vkPhysicalDevice)
    sharedBuffer

  def render(): Unit =
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, sharedBuffer.glBufferHandle)
    glBindTexture(GL_TEXTURE_2D, texture)
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_RGBA, GL_HALF_FLOAT, 0L)
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0)

    glClear(GL_COLOR_BUFFER_BIT)
    glUseProgram(shaderProgram)
    glBindVertexArray(vao)
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D, texture)
    glDrawArrays(GL_TRIANGLES, 0, 6)

    glfwSwapBuffers(windowHandle)

  def pollEvents(): Unit = glfwPollEvents()

  def updateMousePosition(): Unit =
    val stack = MemoryStack.stackPush()
    try
      val xBuf = stack.mallocDouble(1)
      val yBuf = stack.mallocDouble(1)
      glfwGetCursorPos(windowHandle, xBuf, yBuf)
      _mouseX = xBuf.get(0)
      _mouseY = yBuf.get(0)
    finally stack.pop()

  def normalizedMouseX: Float = ((_mouseX / width) * 2.0 - 1.0).toFloat
  def normalizedMouseY: Float = (1.0 - (_mouseY / height) * 2.0).toFloat
  def shouldClose: Boolean = glfwWindowShouldClose(windowHandle)

  override def close(): Unit =
    if sharedBuffer != null then sharedBuffer.close()
    glDeleteTextures(texture)
    glDeleteVertexArrays(vao)
    glDeleteProgram(shaderProgram)
    glfwDestroyWindow(windowHandle)
    glfwTerminate()

  private def createShaderProgram(): Int =
    val vs = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(vs, """#version 450 core
      |out vec2 uv;
      |void main() {
      |    vec2 positions[6] = vec2[](vec2(-1,-1),vec2(1,-1),vec2(1,1),vec2(-1,-1),vec2(1,1),vec2(-1,1));
      |    vec2 texCoords[6] = vec2[](vec2(0,1),vec2(1,1),vec2(1,0),vec2(0,1),vec2(1,0),vec2(0,0));
      |    gl_Position = vec4(positions[gl_VertexID], 0.0, 1.0);
      |    uv = texCoords[gl_VertexID];
      |}""".stripMargin)
    glCompileShader(vs)

    val fs = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(fs, """#version 450 core
      |in vec2 uv;
      |out vec4 fragColor;
      |uniform sampler2D tex;
      |void main() { fragColor = texture(tex, uv); }""".stripMargin)
    glCompileShader(fs)

    val program = glCreateProgram()
    glAttachShader(program, vs)
    glAttachShader(program, fs)
    glLinkProgram(program)
    glDeleteShader(vs)
    glDeleteShader(fs)
    program
