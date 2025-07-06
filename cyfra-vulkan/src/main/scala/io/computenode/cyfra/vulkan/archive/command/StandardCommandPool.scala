package io.computenode.cyfra.vulkan.archive.command

import io.computenode.cyfra.vulkan.archive.core.Device

/** @author
  *   MarconZet Created 13.04.2020 Copied from Wrap
  */
private[cyfra] class StandardCommandPool(device: Device, queue: Queue) extends CommandPool(device, queue):
  protected def getFlags: Int = 0
