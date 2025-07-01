package io.computenode.cyfra.core

import io.computenode.cyfra.core.GExecution.*
import io.computenode.cyfra.core.aalegacy.GContext
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.binding.GBuffer
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.UniformStructRef
import izumi.reflect.Tag
import GExecution.*

trait GExecution[-Params, -L <: Layout, +RL <: Layout]:

  def flatMap[NRL <: Layout, NP <: Params, NL <: L](f: RL => GExecution[NP, NL, NRL]): GExecution[NP, NL, NRL] =
    FlatMap(this, (p, r) => f(r))

  def mapResult[NRL <: Layout](f: RL => NRL): GExecution[Params, L, NRL] =
    Map(this, f, identity, identity)

  def contramapLayout[NL <: Layout](f: NL => L): GExecution[Params, NL, RL] =
    Map(this, identity, f, identity)

  def contramapParams[NP](f: NP => Params): GExecution[NP, L, RL] =
    Map(this, identity, identity, f)

  def addProgram[ProgramParams, PP <: Params, ProgramLayout <: Layout, PL <: L, P <: GProgram[ProgramParams, ProgramLayout]](
    program: P,
  )(mapParams: PP => ProgramParams, mapLayout: PL => ProgramLayout): GExecution[PP, PL, ProgramLayout] =
    val adapted = program.contramapParams(mapParams).contramapLayout(mapLayout)
    flatMap(_ => adapted)

object GExecution:

  def apply[Params, L <: Layout]() =
    Pure[Params, L, L]()
    
  def forParams[Params, L <: Layout, RL <: Layout](
    f: Params => GExecution[Params, L, RL],
  ): GExecution[Params, L, RL] =
    FlatMap[Params, L, RL, RL](
      Pure[Params, L, RL](),
      (params: Params, _: RL) => f(params),
    )
    
  case class Pure[Params, L <: Layout, RL <: Layout]() extends GExecution[Params, L, RL]
    
  case class FlatMap[Params, L <: Layout, RL <: Layout, NRL <: Layout](
    execution: GExecution[Params, L, RL],
    f: (Params, RL) => GExecution[Params, L, NRL],
  ) extends GExecution[Params, L, NRL]
  
  case class Map[P, NP, L <: Layout, NL <: Layout, RL <: Layout, NRL <: Layout](
    execution: GExecution[P, L, RL],
    mapResult: RL => NRL,
    contramapLayout: NL => L,
    contramapParams: NP => P,
  ) extends GExecution[NP, NL, NRL]:

    override def mapResult[NNRL <: Layout](f: NRL => NNRL): GExecution[NP, NL, NNRL] =
      Map(execution, mapResult andThen f, contramapLayout, contramapParams)

    override def contramapParams[NNP](f: NNP => NP): GExecution[NNP, NL, NRL] =
      Map(execution, mapResult, contramapLayout, f andThen contramapParams)

    override def contramapLayout[NNL <: Layout](f: NNL => NL): GExecution[NP, NNL, NRL] =
      Map(execution, mapResult, f andThen contramapLayout, contramapParams)
