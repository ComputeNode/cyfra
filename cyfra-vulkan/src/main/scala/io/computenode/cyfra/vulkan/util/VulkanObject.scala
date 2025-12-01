package io.computenode.cyfra.vulkan.util

import org.lwjgl.system.Pointer

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] abstract class VulkanObject[T]:
  protected val handle: T
  private var alive: Boolean = true
  def isAlive: Boolean = alive

  def get: T =
    if !alive then throw new IllegalStateException()
    else handle

  def destroy(): Unit =
    if !alive then throw new IllegalStateException()
    close()
    alive = false

  protected def close(): Unit

  override def toString: String =
    val className = this.getClass.getSimpleName
    val hex = handle match
      case p: Pointer => p.address().toHexString
      case l: Long    => l.toHexString
      case _          => super.toString

    s"$className 0x$hex"
