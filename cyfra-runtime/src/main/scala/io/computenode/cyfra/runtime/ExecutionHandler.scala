package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{GExecution, GProgram}
import io.computenode.cyfra.core.layout.Layout

object ExecutionHandler:

  def handle[Params, L <: Layout, RL <: Layout](execution: GExecution[Params, L, RL], params: Params): List[BoundProgram[?, ?, ?]] =
    ???

  case class BoundProgram[LParams, Params, L <: Layout](layout: L, paramsMapping: LParams => Params, program: GProgram[Params, L])
