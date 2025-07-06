package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.Allocation
import io.computenode.cyfra.core.{Allocation, CyfraRuntime}

class VkCyfraRuntime extends CyfraRuntime:
  override def allocation(): Allocation =
    new VkAllocation()
