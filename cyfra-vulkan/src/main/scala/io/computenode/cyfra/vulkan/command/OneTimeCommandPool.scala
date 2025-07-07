package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.core.Device
import org.lwjgl.vulkan.VK10.VK_COMMAND_POOL_CREATE_TRANSIENT_BIT

/** @author
  *   MarconZet Created 13.04.2020 Copied from Wrap
  */
private[cyfra] class OneTimeCommandPool(queue: Queue)(using device: Device) extends CommandPool(queue)(using device: Device):
  protected def getFlags: Int = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
