package io.computenode.cyfra.fotonviz

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct

/** Input state passed to GPU programs.
  *
  * @param mouseX
  *   Normalized mouse X coordinate [-1, 1]
  * @param mouseY
  *   Normalized mouse Y coordinate [-1, 1]
  * @param mousePressed
  *   1 if mouse is pressed, 0 otherwise
  * @param time
  *   Elapsed time in seconds
  * @param deltaTime
  *   Time since last frame in seconds
  * @param frameIndex
  *   Current frame number
  */
case class InputState(
  mouseX: Float32,
  mouseY: Float32,
  mousePressed: Int32,
  time: Float32,
  deltaTime: Float32,
  frameIndex: Int32,
) extends GStruct[InputState]

object InputState:
  /** Create an InputState from window state */
  def fromWindow(window: VizWindow, time: Float, deltaTime: Float, frameIndex: Int): InputState =
    InputState(
      mouseX = window.normalizedMouseX,
      mouseY = window.normalizedMouseY,
      mousePressed = if window.mousePressed then 1 else 0,
      time = time,
      deltaTime = deltaTime,
      frameIndex = frameIndex,
    )
