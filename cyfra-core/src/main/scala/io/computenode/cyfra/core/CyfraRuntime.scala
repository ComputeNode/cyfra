package io.computenode.cyfra.core

import io.computenode.cyfra.core.Allocation.{FinalizeAlloc, InitAlloc}

trait CyfraRuntime:

  def allocation(): Allocation

  def initAlloc(allocation: Allocation): InitAlloc

  def finalizeAlloc(allocation: Allocation): FinalizeAlloc
