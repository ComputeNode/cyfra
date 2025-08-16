package io.computenode.cyfra.vulkan.util

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] abstract class VulkanObject[T]:
  protected val handle: T
  private var alive: Boolean = true

  def get: T =
    if !alive then throw new IllegalStateException()
    else handle

  def destroy(): Unit =
    if !alive then throw new IllegalStateException()
    close()
    alive = false

  protected def close(): Unit
