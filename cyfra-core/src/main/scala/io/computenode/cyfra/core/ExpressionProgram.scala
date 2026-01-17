package io.computenode.cyfra.core

import io.computenode.cyfra.core.GProgram.*
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.core.expression.ExpressionBlock
import izumi.reflect.Tag

case class GioProgram[Params, L: Layout](
  body: L => ExpressionBlock[Unit],
  layout: InitProgramLayout => Params => L,
  dispatch: (L, Params) => ProgramDispatch,
  workgroupSize: WorkDimensions,
) extends GProgram[Params, L]
