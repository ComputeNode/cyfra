package io.computenode.cyfra.samples

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.Value.{GBoolean, Int32}
import io.computenode.cyfra.dsl.buffer.GBuffer
import io.computenode.cyfra.dsl.gio.GIO

object TestingStuff:
  @main
  def test =

    case class EmitProgramParams(
      inSize: Int,
      emitN: Int
    )
    
    case class EmitProgramLayout(
      in: GBuffer[Int32],
      out: GBuffer[Int32]
    ) extends Layout
    
    val emitProgram = GProgram[EmitProgramParams, EmitProgramLayout](
      params => EmitProgramLayout(
        in = GBuffer[Int32](params.inSize),
        out = GBuffer[Int32](params.emitN)
      )) { layout =>
        for
          _ <- GIO.pure(2)
        yield ()
      }
          
      
      
    