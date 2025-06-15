package io.computenode.cyfra.rtrp.window

import io.computenode.cyfra.window.core._
import scala.util.{Try, Success, Failure}

object WindowSystemExample {
  
  def main(args: Array[String]): Unit = {
    println("Starting Window System Example")
    
    val result = WindowManager.withManager { manager =>
      runExample(manager)
    }
    
    result match {
      case Success(_) => println("Example completed successfully")
      case Failure(ex) => 
        println(s"Example failed: ${ex.getMessage}")
        ex.printStackTrace()
    }
  }
  
  private def runExample(manager: WindowManager): Try[Unit] = Try {
    manager.onWindowClose { event =>
      println(s"Window ${event.windowId} close requested")
    }
    
    manager.onWindowResize { event =>
      println(s"Window ${event.windowId} resized to ${event.width}x${event.height}")
    }
    
    manager.onKeyPress { event =>
      println(s"Key pressed: ${event.key.code} in window ${event.windowId}")
    }
    
    manager.onMouseClick { event =>
      println(s"Mouse clicked: button ${event.button.code} at (${event.x}, ${event.y}) in window ${event.windowId}")
    }
    
    // Create a window
    val window = manager.createWindow { config =>
      config.copy(
        width = 1024,
        height = 768,
        title = "Window Example",
        position = Some(WindowPosition.Centered)
      )
    }.get
    
    println(s"Created window: ${window.id}")
    
    // Main loop
    var running = true
    var frameCount = 0
    
    while (running && !window.shouldClose) {
      // Poll and handle events
      manager.pollAndDispatchEvents()
      
      // Simple frame counter
      frameCount += 1
      if (frameCount % 60 == 0) {
        println(s"Frame $frameCount - Window active: ${window.isVisible}")
      }
      
      // Simulate some work (in real, this would be rendering)
      Thread.sleep(16) // ~60 FPS
      
      if (frameCount >= 1000) {
        running = false
      }
    }
    
    println("Main loop ended")
  }
}