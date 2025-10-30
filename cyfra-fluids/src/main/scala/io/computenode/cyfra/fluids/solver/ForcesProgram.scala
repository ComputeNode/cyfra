package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty

object ForcesProgram:

  def create: GProgram[Int, FluidState] =
    GProgram[Int, FluidState](
      layout = totalCells => {
        import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
        FluidState(
          velocity = GBuffer[Vec4[Float32]](totalCells),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          obstacles = GBuffer[Float32](totalCells),
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
        // Read values
        val oldVel = GIO.read(state.velocity, idx)
        val temp = GIO.read(state.temperature, idx)
        
        // Compute buoyancy force + wind (Vec4 with w=0)
        val forces = vec4(
          params.windX,
          params.buoyancy * (temp - params.ambient) + params.windY,
          params.windZ,
          0.0f
        )
        val newVel = oldVel + forces * params.dt
        
        // Write result
        GIO.write(state.velocity, idx, newVel)
