package io.computenode.cyfra.vulkan.util

/** @author
  *   MarconZet Created 13.04.2020
  */
abstract private[cyfra] class VulkanObjectHandle extends VulkanObject {
  protected val handle: Long

  def get: Long =
    if (!alive)
      throw new IllegalStateException()
    else
      handle
}
