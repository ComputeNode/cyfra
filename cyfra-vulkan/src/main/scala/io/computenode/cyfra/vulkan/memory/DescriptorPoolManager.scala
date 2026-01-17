package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.core.Device

class DescriptorPoolManager(using Device):
  private val freePools: collection.mutable.Queue[DescriptorPool] = collection.mutable.Queue.empty

  def allocate(): DescriptorPool = synchronized:
    freePools.removeHeadOption() match
      case Some(value) => value
      case None        => new DescriptorPool(100)

  def free(pools: DescriptorPool*): Unit = synchronized:
    pools.foreach(_.reset())
    freePools.enqueueAll(pools)

  def destroy(): Unit = synchronized:
    freePools.foreach(_.destroy())
    freePools.clear()
