package io.computenode.cyfra.core

import io.computenode.cyfra.core.GProgram.*
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.Value.GBoolean
import io.computenode.cyfra.dsl.gio.GIO
import izumi.reflect.Tag

case class GioProgram[Params, L: Layout](
  body: L => GIO[?],
  layout: InitProgramLayout => Params => L,
  dispatch: (L, Params) => ProgramDispatch,
  workgroupSize: WorkDimensions,
) extends GProgram[Params, L]
