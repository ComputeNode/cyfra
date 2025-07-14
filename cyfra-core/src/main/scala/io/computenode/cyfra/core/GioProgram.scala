package io.computenode.cyfra.core

import io.computenode.cyfra.core.GProgram.*
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.Value.GBoolean
import io.computenode.cyfra.dsl.gio.GIO
import izumi.reflect.Tag

case class GioProgram[Params, L <: Layout: LayoutStruct](
  body: L => GIO[?],
  layout: InitProgramLayout => Params => L,
  dispatch: (L, Params) => ProgramDispatch,
  workgroupSize: WorkDimensions,
) extends GProgram[Params, L]:
  private[cyfra] def cacheKey: String = layoutStruct.elementTypes match
    case x if x.size == 12                                     => "addOne"
    case x if x.contains(summon[Tag[GBoolean]]) && x.size == 3 => "filter"
    case x if x.size == 3                                      => "emit"
    case _                                                     => ???
