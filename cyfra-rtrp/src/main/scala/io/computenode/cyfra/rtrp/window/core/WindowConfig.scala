package io.computenode.cyfra.window.core

case class WindowConfig(
  width: Int = 800,
  height: Int = 600,
  title: String = "Cyfra Window",
  resizable: Boolean = true,
  decorated: Boolean = true,
  fullscreen: Boolean = false,
  vsync: Boolean = true,
  samples: Int = 1, // MSAA samples
  position: Option[WindowPosition] = None
)

sealed trait WindowPosition
object WindowPosition {
  case object Centered extends WindowPosition
  case class Fixed(x: Int, y: Int) extends WindowPosition
  case object Default extends WindowPosition
}

case class WindowProperties(
  width: Int,
  height: Int,
  title: String,
  visible: Boolean,
  focused: Boolean,
  minimized: Boolean,
  maximized: Boolean
)