package io.computenode.cyfra.vulkan.util

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] abstract class VulkanObjectHandle extends VulkanObject {
  protected val handle: Long

  def get: Long =
    if (!alive)
      throw new IllegalStateException()
    else
      handle
}
