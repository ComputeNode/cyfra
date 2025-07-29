package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.compute.ComputePipeline.DescriptorSetLayout

import scala.annotation.tailrec
import scala.collection.mutable

class DescriptorSetManager(poolManager: DescriptorPoolManager) {
  private var currentPool: Option[DescriptorPool] = None
  private val exhaustedPools = mutable.Buffer.empty[DescriptorPool]
  private val freeSets = mutable.HashMap.empty[DescriptorSetLayout, mutable.Queue[DescriptorSet]]

  def allocate(layout: DescriptorSetLayout): DescriptorSet =
    freeSets.get(layout).flatMap(_.removeHeadOption(true)).getOrElse(allocateNew(layout))

  def free(descriptorSet: DescriptorSet): Unit =
    freeSets.getOrElseUpdate(descriptorSet.layout, mutable.Queue.empty) += descriptorSet

  def destroy(): Unit = {
    currentPool.foreach(poolManager.free(_))
    poolManager.free(exhaustedPools.toSeq*)
    currentPool = None
    exhaustedPools.clear()
  }

  @tailrec
  private def allocateNew(layout: DescriptorSetLayout): DescriptorSet =
    currentPool.flatMap(_.allocate(layout)) match {
      case Some(value) => value
      case None        =>
        currentPool.foreach(exhaustedPools += _)
        currentPool = Some(poolManager.allocate())
        this.allocateNew(layout)
    }

}
