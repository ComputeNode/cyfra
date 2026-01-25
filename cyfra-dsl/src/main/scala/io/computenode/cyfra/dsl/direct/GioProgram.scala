package io.computenode.cyfra.dsl.direct

import io.computenode.cyfra.core.{ExpressionProgram, GProgram, Layout}
import io.computenode.cyfra.core.GProgram.{InitProgramLayout, ProgramDispatch, WorkDimensions}
import io.computenode.cyfra.core.expression.{BuildInFunction, CustomFunction, Expression, ExpressionBlock, JumpTarget, Value, given}
import io.computenode.cyfra.core.expression.CustomFunction.CustomFunction1
import io.computenode.cyfra.core.binding.{GBuffer, Variable}
import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.core.expression.Value.irs
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.dsl.direct.GIO.reify
import io.computenode.cyfra.dsl.direct.GIO

object GioProgram:
  def apply[Params, L: Layout](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    workgroupSize: WorkDimensions = (128, 1, 1),
  )(body: L => GIO ?=> Unit): GProgram[Params, L] =
    val nBody = (layout: L) => reify(body(layout))
    new ExpressionProgram[Params, L](nBody, s => layout(using s), dispatch, workgroupSize)
