package io.computenode.cyfra.rtrp.window

import io.computenode.cyfra.rtrp.window.core.*
import io.computenode.cyfra.rtrp.window.platform.GLFWWindowSystem
import io.computenode.cyfra.rtrp.CyfraRtrpException
import scala.util.*

class WindowManager:
  private var windowSystem: Option[WindowSystem] = None
  private var eventHandlers: Map[Class[? <: WindowEvent], WindowEvent => Unit] = Map.empty

  def initialize(): Try[Unit] =
    if windowSystem.isDefined then return Failure(WindowSystemInitializationException("WindowManager already initialized"))

    val glfwSystem = new GLFWWindowSystem()
    glfwSystem
      .initialize()
      .map: _ =>
        windowSystem = Some(glfwSystem)

  def shutdown(): Try[Unit] =
    windowSystem match
      case Some(system) =>
        val result = system.shutdown()
        windowSystem = None
        result
      case None =>
        Success(())

  def createWindow(): Try[Window] =
    createWindow(WindowConfig())

  def createWindow(config: WindowConfig): Try[Window] =
    windowSystem match
      case Some(system) => system.createWindow(config)
      case None         => Failure(WindowSystemNotInitializedException("WindowManager not initialized"))

  def createWindow(configure: WindowConfig => WindowConfig): Try[Window] =
    val config = configure(WindowConfig())
    createWindow(config)

  def pollAndDispatchEvents(): Try[Unit] =
    windowSystem match
      case Some(system) =>
        system.pollEvents().map { events =>
          events.foreach(dispatchEvent)
        }
      case None =>
        Failure(WindowSystemNotInitializedException("WindowManager not initialized"))

  def onEvent[T <: WindowEvent](eventClass: Class[T])(handler: T => Unit): Unit =
    eventHandlers = eventHandlers + (eventClass -> handler.asInstanceOf[WindowEvent => Unit])

  def onWindowClose(handler: WindowEvent.CloseRequested => Unit): Unit =
    onEvent(classOf[WindowEvent.CloseRequested])(handler)

  def onWindowResize(handler: WindowEvent.Resized => Unit): Unit =
    onEvent(classOf[WindowEvent.Resized])(handler)

  def onKeyPress(handler: InputEvent.KeyPressed => Unit): Unit =
    onEvent(classOf[InputEvent.KeyPressed])(handler)

  def onMouseClick(handler: InputEvent.MousePressed => Unit): Unit =
    onEvent(classOf[InputEvent.MousePressed])(handler)

  def getActiveWindows(): List[Window] =
    windowSystem.map(_.getActiveWindows()).getOrElse(List.empty)

  def isInitialized: Boolean = windowSystem.isDefined

  private def dispatchEvent(event: WindowEvent): Unit =
    eventHandlers.get(event.getClass).foreach(_(event))

object WindowManager:

  def create(): Try[WindowManager] =
    val manager = new WindowManager()
    manager.initialize().map(_ => manager)

  def withManager[T](action: WindowManager => Try[T]): Try[T] =
    create().flatMap: manager =>
      try
        action(manager)
      finally
        manager.shutdown()
