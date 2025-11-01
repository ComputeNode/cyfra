package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.fluids.solver.GridUtils.{idxTo3D, coord3dToIdx}

/** Pure Neumann boundary conditions matching GPU Gems Chapter 38.
  * 
  * Implements ∂u/∂n = 0 at boundaries (zero gradient normal to boundary).
  * In practice: boundary cells copy values from their interior neighbors.
  * 
  * This is simpler than free-slip or outflow - just ensures smooth flow at edges.
  */
object NeumannBoundaryProgram:

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
      val n = params.gridSize
      val totalCells = n * n * n

      GIO.when(idx < totalCells):
        val (x, y, z) = idxTo3D(idx, n)
        
        // Check if this cell is on any boundary
        val onBoundary = (x === 0) || (x === n - 1) || 
                        (y === 0) || (y === n - 1) || 
                        (z === 0) || (z === n - 1) ||
                        (ObstacleUtils.isSolid(state.obstacles, idx, totalCells))
        
        // Only apply boundary condition to boundary cells
        GIO.when(onBoundary):
          // Pure Neumann BC: ∂u/∂n = 0
          // Copy from interior neighbor (one step inward from boundary)
          
          // Determine interior neighbor coordinates
          val xInterior = when(x === 0)(1: Int32).elseWhen(x === n - 1)(n - 2).otherwise(x)
          val yInterior = when(y === 0)(1: Int32).elseWhen(y === n - 1)(n - 2).otherwise(y)
          val zInterior = when(z === 0)(1: Int32).elseWhen(z === n - 1)(n - 2).otherwise(z)
          
          val interiorIdx = coord3dToIdx(xInterior, yInterior, zInterior, n)
          
          // Copy all fields from interior
          val interiorVel = state.velocity.read(interiorIdx)
          val interiorDensity = state.density.read(interiorIdx)
          val interiorTemp = state.temperature.read(interiorIdx)
          val interiorPressure = state.pressure.read(interiorIdx)
          
          for
            _ <- GIO.write(state.velocity, idx, interiorVel)
            _ <- GIO.write(state.density, idx, interiorDensity)
            _ <- GIO.write(state.temperature, idx, interiorTemp)
            _ <- GIO.write(state.pressure, idx, interiorPressure)
          yield Empty()


