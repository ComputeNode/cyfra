package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.Allocation.{FinalizeAlloc, InitAlloc}
import io.computenode.cyfra.core.{Allocation, CyfraRuntime}

class VkCyfraRuntime extends CyfraRuntime:
  override def allocation(): Allocation =
    new VkAllocation()

  override def initAlloc(allocation: Allocation): InitAlloc =
    ???
    
  override def finalizeAlloc(allocation: Allocation): FinalizeAlloc =
    ???