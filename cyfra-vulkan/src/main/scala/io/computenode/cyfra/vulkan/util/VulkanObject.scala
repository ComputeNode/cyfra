package io.computenode.cyfra.vulkan.util

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] abstract class VulkanObject {
  protected var alive: Boolean = true

  def destroy(): Unit = {
    if (!alive)
      throw new IllegalStateException()
    close()
    alive = false
  }

  protected def close(): Unit

}
