package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.core.Device

private[cyfra] class ResettableCommandPool(device: Device, queue: Queue) extends CommandPool(device, queue):
  protected def getFlags: Int = 2
