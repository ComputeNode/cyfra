package io.computenode.cyfra.core

import io.computenode.cyfra.core.GExecution.*
import io.computenode.cyfra.core.aalegacy.GContext
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.buffer.GBuffer
import io.computenode.cyfra.dsl.gio.GIO

case class GExecution[Params, L <: Layout](layoutStruct: LayoutStruct[L], boundPrograms: Seq[BoundProgram[?]]):
  def execute(layout: L)(using Allocation): L =
    println("Executing GExecution...")
    layout // Return the layout after execution

object GExecution:

  def forLayout[Params, L <: Layout](using layoutStruct: LayoutStruct[L]): GExecutionBuilder[Params, L] =
    GExecutionBuilder(layoutStruct, Seq.empty)
  
  case class GExecutionBuilder[Params, L <: Layout](layoutStruct: LayoutStruct[L], boundPrograms: Seq[BoundProgram[?]]):
    def addProgram[ProgramLayout <: Layout, ProgramParams, P <: GProgram[ProgramParams, ProgramLayout]](
      program: P,
    )(mapLayout: L => ProgramBinding[ProgramLayout], mapParams: Params => ProgramParams): GExecutionBuilder[Params, L] =
      val ProgramBinding(mappedLayout, dispatch) = mapLayout(layoutStruct.layoutRef)
      val boundProgram = BoundProgram(mappedLayout, dispatch, program.body(mappedLayout))
      GExecutionBuilder(layoutStruct, boundPrograms :+ boundProgram)

    def compile(using GContext): GExecution[Params, L] =
      println("Compiling GExecution...")
      GExecution(layoutStruct, boundPrograms)

  type WorkDimensions = (Int, Int, Int)

  sealed trait ProgramDispatch

  case class DynamicDispatch[L <: Layout](buffer: GBuffer[?], offset: Int) extends ProgramDispatch

  case class StaticDispatch[L <: Layout](size: WorkDimensions) extends ProgramDispatch

  case class ProgramBinding[L <: Layout](programLayout: L, programDispatch: ProgramDispatch)

  case class BoundProgram[L <: Layout](layout: L, dispatch: ProgramDispatch, body: GIO[?])

