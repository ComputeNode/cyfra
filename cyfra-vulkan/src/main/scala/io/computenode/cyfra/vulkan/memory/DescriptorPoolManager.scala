package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.core.Device

class DescriptorPoolManager(using Device) {
  private val freePools: collection.mutable.Queue[DescriptorPool] = collection.mutable.Queue.empty

  def allocate(): DescriptorPool = synchronized:
    freePools.removeHeadOption() match {
      case Some(value) => value
      case None        => new DescriptorPool()
    }
  def free(pool: DescriptorPool*): Unit = synchronized:
    freePools.enqueueAll(pool)

  def destroy(): Unit = synchronized:
    freePools.foreach(_.destroy())
    freePools.clear()

}
