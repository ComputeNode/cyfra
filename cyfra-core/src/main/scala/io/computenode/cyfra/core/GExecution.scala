package io.computenode.cyfra.core

import io.computenode.cyfra.core.GExecution.*
import io.computenode.cyfra.core.aalegacy.GContext
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.buffer.GBuffer
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.UniformStructRef
import izumi.reflect.Tag

case class GExecution[Params, L <: Layout, RL <: Layout](
  layoutStruct: LayoutStruct[L],
  boundPrograms: Seq[BoundProgram[Params, ?, ?, ?]],
  toResult: L => RL,
):
  def execute(layout: L, params: Params)(using Allocation): RL =
    println("Executing GExecution...")
    toResult(layout)

object GExecution:

  def build[Params, L <: Layout, RL <: Layout](using layoutStruct: LayoutStruct[L]): GExecutionBuilder[Params, L, RL] =
    GExecutionBuilder(layoutStruct, Seq.empty)

  case class GExecutionBuilder[Params, L <: Layout, RL <: Layout](layoutStruct: LayoutStruct[L], boundPrograms: Seq[BoundProgram[Params, ?, ?, ?]]):
    def addProgram[ProgramParams, Uniform <: GStruct[Uniform]: GStructSchema: Tag, ProgramLayout <: Layout, P <: GProgram[
      ProgramParams,
      Uniform,
      ProgramLayout,
    ]](program: P)(mapLayout: L => ProgramLayout, mapParams: Params => ProgramParams): GExecutionBuilder[Params, L, RL] =
      val mappedLayout = mapLayout(layoutStruct.layoutRef)
      val boundProgram = BoundProgram(mappedLayout, mapParams, program)
      GExecutionBuilder(layoutStruct, boundPrograms :+ boundProgram)

    def compile(toResult: L => RL)(using GContext): GExecution[Params, L, RL] =
      println("Compiling GExecution...")
      GExecution(layoutStruct, boundPrograms, toResult)

  case class BoundProgram[LParams, Params, Uniform <: GStruct[Uniform], L <: Layout](
    layout: L,
    paramsMapping: LParams => Params,
    program: GProgram[Params, Uniform, L],
  )
