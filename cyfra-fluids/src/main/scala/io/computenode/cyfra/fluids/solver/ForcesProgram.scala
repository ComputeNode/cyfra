package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}

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
        // Read values (pure operations)
        val oldVel = GIO.read(state.velocity, idx)
        val temp = GIO.read(state.temperature, idx)
        
        // Debug: Print first few cells
        GIO.when(idx < 3):
          GIO.printf("Forces[%d]: temp=%f, oldVel.y=%f\n", idx, temp, oldVel.y)
        
        // Compute buoyancy force + wind (Vec4 with w=0)
        val buoyancyForce = vec4(
          params.windX,
          params.buoyancy * (temp - params.ambient),
          params.windZ,
          0.0f
        )
        val newVel = oldVel + buoyancyForce * params.dt
        
        // Debug: Print first few results
        GIO.when(idx < 3):
          GIO.printf("  -> newVel.y=%f, buoyancy=%f, dt=%f\n", newVel.y, params.buoyancy, params.dt)
        
        // Write result
        GIO.write(state.velocity, idx, newVel)
