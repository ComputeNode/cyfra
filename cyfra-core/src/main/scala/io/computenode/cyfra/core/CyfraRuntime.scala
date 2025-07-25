package io.computenode.cyfra.core

import io.computenode.cyfra.core.Allocation

trait CyfraRuntime:

  def withAllocation(f: Allocation => Unit): Unit
