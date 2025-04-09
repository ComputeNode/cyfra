package io.computenode.cyfra.window

import org.lwjgl.glfw.{GLFW, GLFWWindowSizeCallback, GLFWKeyCallback, GLFWMouseButtonCallback, 
  GLFWCursorPosCallback, GLFWFramebufferSizeCallback, GLFWWindowCloseCallback, 
  GLFWCharCallback, GLFWScrollCallback}
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.system.MemoryStack
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._
import scala.util.{Try, Success, Failure}

/**
 * GLFW implementation of WindowSystem interface.
 */
class GLFWWindowSystem extends WindowSystem {
  // Thread-safe event queue to collect events between polls
  private val eventQueue = new ConcurrentLinkedQueue[WindowEvent]()
  
  // Initialize GLFW first
  GLFWSystem.initializeGLFW() match {
    case Failure(exception) => throw exception
    case Success(_) => // GLFW initialized successfully
  }

  /**
   * Applies window hints for GLFW window creation.
   * This method centralizes all window configuration options.
   */
  private def applyWindowHints(): Unit = {
    // Core window hints
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)  // No OpenGL context, using Vulkan
    GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, GLFW.GLFW_TRUE)     // Window resizable
    
    // Platform-specific hints
    val osName = System.getProperty("os.name").toLowerCase
    
    if (osName.contains("mac")) {
      // macOS specific hints
      GLFW.glfwWindowHint(GLFW.GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW.GLFW_FALSE)
      GLFW.glfwWindowHint(GLFW.GLFW_COCOA_GRAPHICS_SWITCHING, GLFW.GLFW_TRUE)
    } else if (osName.contains("win")) {
      // Windows-specific hints
      GLFW.glfwWindowHint(GLFW.GLFW_SCALE_TO_MONITOR, GLFW.GLFW_TRUE)
    } else if (osName.contains("linux") || osName.contains("unix")) {
      // Linux specific hints
      GLFW.glfwWindowHint(GLFW.GLFW_FOCUS_ON_SHOW, GLFW.GLFW_TRUE)
    }
  }

  /**
   * Creates a new GLFW window with specified dimensions and title.
   *
   * @param width The width of the window in pixels
   * @param height The height of the window in pixels
   * @param title The window title
   * @return A handle to the created window
   */
  override def createWindow(width: Int, height: Int, title: String): WindowHandle = {
    // Apply window hints before creating the window
    applyWindowHints()
    
    // Create the window
    val windowPtr = GLFW.glfwCreateWindow(width, height, title, NULL, NULL)
    if (windowPtr == NULL) {
      throw new RuntimeException("Failed to create GLFW window")
    }
    
    val handle = new GLFWWindowHandle(windowPtr)
    
    // Register callbacks for this window
    setupCallbacks(handle)
    
    // Position window in the center of the primary monitor
    val vidMode = GLFW.glfwGetVideoMode(GLFW.glfwGetPrimaryMonitor())
    GLFW.glfwSetWindowPos(
      windowPtr,
      (vidMode.width() - width) / 2,
      (vidMode.height() - height) / 2
    )
    
    // Make the window visible
    GLFW.glfwShowWindow(windowPtr)
    
    handle
  }

  /**
   * Polls for and returns pending window events.
   *
   * @return List of window events that occurred since the last poll
   */
  override def pollEvents(): List[WindowEvent] = {
    // Poll for events
    GLFW.glfwPollEvents()
    
    // Drain the event queue to a list
    val events = eventQueue.asScala.toList
    eventQueue.clear()
    events
  }

  /**
   * Checks if a window should close.
   *
   * @param window The window handle to check
   * @return true if the window should close, false otherwise
   */
  override def shouldWindowClose(window: WindowHandle): Boolean = {
    GLFW.glfwWindowShouldClose(window.nativePtr)
  }

  /**
   * Sets up GLFW callbacks for the given window handle.
   * Callbacks will populate the eventQueue with WindowEvents.
   */
  private def setupCallbacks(window: WindowHandle): Unit = {
    val windowPtr = window.nativePtr
    
    // Window framebuffer size callback (for handling DPI changes)
    GLFW.glfwSetFramebufferSizeCallback(windowPtr, new GLFWFramebufferSizeCallback {
      override def invoke(window: Long, width: Int, height: Int): Unit = {
        eventQueue.add(WindowEvent.Resize(width, height))
      }
    })
    
    // Window size callback
    GLFW.glfwSetWindowSizeCallback(windowPtr, new GLFWWindowSizeCallback {
      override def invoke(window: Long, width: Int, height: Int): Unit = {
        eventQueue.add(WindowEvent.Resize(width, height))
      }
    })
    
    // Key callback
    GLFW.glfwSetKeyCallback(windowPtr, new GLFWKeyCallback {
      override def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int): Unit = {
        eventQueue.add(WindowEvent.Key(key, action, mods))
      }
    })
    
    // Character input callback (for text input)
    GLFW.glfwSetCharCallback(windowPtr, new GLFWCharCallback {
      override def invoke(window: Long, codepoint: Int): Unit = {
        eventQueue.add(WindowEvent.CharInput(codepoint))
      }
    })
    
    // Mouse button callback - FIX THE BUFFER ALLOCATION
    GLFW.glfwSetMouseButtonCallback(windowPtr, new GLFWMouseButtonCallback {
      override def invoke(window: Long, button: Int, action: Int, mods: Int): Unit = {
        // Create the buffers within the scope of this function
        val stack = MemoryStack.stackPush()
        try {
          val xBuffer = stack.mallocDouble(1)
          val yBuffer = stack.mallocDouble(1)
          GLFW.glfwGetCursorPos(window, xBuffer, yBuffer)
          val x = xBuffer.get(0)
          val y = yBuffer.get(0)
          
          eventQueue.add(WindowEvent.MouseButton(button, action == GLFW.GLFW_PRESS, x, y))
        } finally {
          stack.pop()
        }
      }
    })
    
    // Cursor position callback
    GLFW.glfwSetCursorPosCallback(windowPtr, new GLFWCursorPosCallback {
      override def invoke(window: Long, x: Double, y: Double): Unit = {
        eventQueue.add(WindowEvent.MouseMove(x, y))
      }
    })
    
    // Scroll callback
    GLFW.glfwSetScrollCallback(windowPtr, new GLFWScrollCallback {
      override def invoke(window: Long, xoffset: Double, yoffset: Double): Unit = {
        eventQueue.add(WindowEvent.Scroll(xoffset, yoffset))
      }
    })
    
    // Set close callback
    GLFW.glfwSetWindowCloseCallback(windowPtr, new GLFWWindowCloseCallback {
      override def invoke(window: Long): Unit = {
        eventQueue.add(WindowEvent.Close)
      }
    })
  }
  
  /**
   * Example of how to handle specific events like resize and close.
   *
   * @param events List of events to handle
   */
  def handleEvents(events: List[WindowEvent], window: WindowHandle): Unit = {
    events.foreach {
      case WindowEvent.Resize(width, height) =>
        // Handle resize event, e.g., update viewport
        println(s"Window resized to ${width}x${height}")
      
      case WindowEvent.Close =>
        // Handle close event, e.g., clean up resources
        println("Window close requested")
        GLFW.glfwSetWindowShouldClose(window.nativePtr, true)
      
      case WindowEvent.Key(keyCode, action, mods) =>
        // Handle key events
        val actionName = action match {
          case GLFW.GLFW_PRESS => "pressed"
          case GLFW.GLFW_RELEASE => "released"
          case GLFW.GLFW_REPEAT => "repeated"
          case _ => "unknown"
        }
        println(s"Key $keyCode was $actionName with modifiers $mods")
      
      case _ => // Ignore other events
    }
  }
}