package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.core.Device

/** @author
  *   MarconZet Created 13.04.2020 Copied from Wrap
  */
private[cyfra] class StandardCommandPool( queue: Queue)(using device: Device) extends CommandPool( queue)(using device: Device):
  protected def getFlags: Int = 0
