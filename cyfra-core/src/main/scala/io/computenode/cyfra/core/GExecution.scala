package io.computenode.cyfra.core

import io.computenode.cyfra.core.GExecution.*
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.binding.GBuffer
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag
import GExecution.*

trait GExecution[-Params, ExecLayout: Layout, ResLayout: Layout]:

  def execLayout: Layout[ExecLayout] = Layout[ExecLayout]
  def resLayout: Layout[ResLayout] = Layout[ResLayout]

  def flatMap[NRL: Layout, NP <: Params](f: ResLayout => GExecution[NP, ExecLayout, NRL]): GExecution[NP, ExecLayout, NRL] =
    FlatMap(this, (p, r) => f(r))

  def map[NRL: Layout](f: ResLayout => NRL): GExecution[Params, ExecLayout, NRL] =
    Map(this, f, identity, identity)

  def contramap[NEL: Layout](f: NEL => ExecLayout): GExecution[Params, NEL, ResLayout] =
    Map(this, identity, f, identity)

  def contramapParams[NP](f: NP => Params): GExecution[NP, ExecLayout, ResLayout] =
    Map(this, identity, identity, f)

  def addProgram[ProgramParams, PP <: Params, ProgramLayout: Layout, P <: GProgram[ProgramParams, ProgramLayout]](
    program: P,
  )(mapParams: PP => ProgramParams, mapLayout: ExecLayout => ProgramLayout): GExecution[PP, ExecLayout, ResLayout] =
    val adapted = program.contramapParams(mapParams).contramap(mapLayout)
    flatMap(r => adapted.map(_ => r))

object GExecution:

  def apply[Params, L: Layout]() =
    Pure[Params, L]()

  def forParams[Params, EL: Layout, RL: Layout](f: Params => GExecution[Params, EL, RL]): GExecution[Params, EL, RL] =
    FlatMap[Params, EL, EL, RL](Pure[Params, EL](), (params: Params, _: EL) => f(params))

  case class Pure[Params, L: Layout]() extends GExecution[Params, L, L]

  case class FlatMap[Params, EL: Layout, RL: Layout, NRL: Layout](execution: GExecution[Params, EL, RL], f: (Params, RL) => GExecution[Params, EL, NRL])
      extends GExecution[Params, EL, NRL]

  case class Map[P, NP, EL: Layout, NEL: Layout, RL: Layout, NRL: Layout](
    execution: GExecution[P, EL, RL],
    mapResult: RL => NRL,
    contramapLayout: NEL => EL,
    contramapParams: NP => P,
  ) extends GExecution[NP, NEL, NRL]:

    override def map[NNRL: Layout](f: NRL => NNRL): GExecution[NP, NEL, NNRL] =
      Map(execution, mapResult andThen f, contramapLayout, contramapParams)

    override def contramapParams[NNP](f: NNP => NP): GExecution[NNP, NEL, NRL] =
      Map(execution, mapResult, contramapLayout, f andThen contramapParams)

    override def contramap[NNL: Layout](f: NNL => NEL): GExecution[NP, NNL, NRL] =
      Map(execution, mapResult, f andThen contramapLayout, contramapParams)
