package io.computenode.cyfra.core

import io.computenode.cyfra.core.GExecution.*
import io.computenode.cyfra.core.archive.GContext
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.binding.GBuffer
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.UniformStructRef
import izumi.reflect.Tag
import GExecution.*

trait GExecution[-Params, ExecLayout <: Layout, +ResLayout <: Layout]:

  def flatMap[NRL <: Layout, NP <: Params](f: ResLayout => GExecution[NP, ExecLayout, NRL]): GExecution[NP, ExecLayout, NRL] =
    FlatMap(this, (p, r) => f(r))

  def map[NRL <: Layout](f: ResLayout => NRL): GExecution[Params, ExecLayout, NRL] =
    Map(this, f, identity, identity)

  def contramap[NL <: Layout](f: NL => ExecLayout): GExecution[Params, NL, ResLayout] =
    Map(this, identity, f, identity)

  def contramapParams[NP](f: NP => Params): GExecution[NP, ExecLayout, ResLayout] =
    Map(this, identity, identity, f)

  def addProgram[ProgramParams, PP <: Params, ProgramLayout <: Layout, P <: GProgram[ProgramParams, ProgramLayout]](
    program: P,
  )(mapParams: PP => ProgramParams, mapLayout: ExecLayout => ProgramLayout): GExecution[PP, ExecLayout, ResLayout] =
    val adapted = program.contramapParams(mapParams).contramap(mapLayout)
    flatMap(r => adapted.map(_ => r))

object GExecution:

  def apply[Params, L <: Layout]() =
    Pure[Params, L]()

  def forParams[Params, L <: Layout, RL <: Layout](f: Params => GExecution[Params, L, RL]): GExecution[Params, L, RL] =
    FlatMap[Params, L, L, RL](Pure[Params, L](), (params: Params, _: L) => f(params))

  case class Pure[Params, L <: Layout]() extends GExecution[Params, L, L]

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

    override def map[NNRL <: Layout](f: NRL => NNRL): GExecution[NP, NL, NNRL] =
      Map(execution, mapResult andThen f, contramapLayout, contramapParams)

    override def contramapParams[NNP](f: NNP => NP): GExecution[NNP, NL, NRL] =
      Map(execution, mapResult, contramapLayout, f andThen contramapParams)

    override def contramap[NNL <: Layout](f: NNL => NL): GExecution[NP, NNL, NRL] =
      Map(execution, mapResult, f andThen contramapLayout, contramapParams)
