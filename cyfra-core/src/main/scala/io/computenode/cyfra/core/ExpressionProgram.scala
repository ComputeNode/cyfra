package io.computenode.cyfra.core

import io.computenode.cyfra.core.GProgram.*
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.core.expression.ExpressionBlock
import izumi.reflect.Tag

case class ExpressionProgram[Params, L <: Layout: {LayoutBinding, LayoutStruct}](
  body: L => ExpressionBlock[Unit],
  layout: InitProgramLayout => Params => L,
  dispatch: (L, Params) => ProgramDispatch,
  workgroupSize: WorkDimensions,
) extends GProgram[Params, L]
