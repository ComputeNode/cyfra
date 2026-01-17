package io.computenode.cyfra.fluids.solver.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.*

object ForcesProgram:

  def create: GProgram[Int, FluidState] =
    GProgram[Int, FluidState](
      layout = totalCells => {
        FluidState(
          velocity = GBuffer[Vec4[Float32]](totalCells),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          obstacles = GBuffer[Float32](totalCells),
          dye = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams]()
        )
      },
      dispatch = (_, totalCells) => {
        val workgroupSize = 256
        val numWorkgroups = (totalCells + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1)
    ): state =>
      val idx = GIO.invocationId
      val params = state.params.read
      val totalCells = params.gridSize * params.gridSize * params.gridSize
      
      GIO.when(idx < totalCells):
        val oldVel = GIO.read(state.velocity, idx)
        val temp = GIO.read(state.temperature, idx)
        
        val forces = vec4(
          params.windX,
          params.buoyancy * (temp - params.ambient) + params.windY,
          params.windZ,
          0.0f
        )
        val newVel = oldVel + forces * params.dt
        
        GIO.write(state.velocity, idx, newVel)
