package io.computenode.cyfra.core

import io.computenode.cyfra.core.GProgram.*
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.Value.GBoolean
import io.computenode.cyfra.dsl.gio.GIO
import izumi.reflect.Tag

/** A GProgram that has a DSL body (compiled to SPIR-V at runtime). */
case class GioProgram[Params, L: Layout](
  body: L => GIO[?],
  layout: InitProgramLayout => Params => L,
  dispatchSize: (L, Params) => ProgramDispatch,
  workgroupSize: WorkDimensions,
  name: String,
) extends GProgram[Params, L]
