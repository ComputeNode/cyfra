package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}

/** Applies no-slip boundary conditions at domain walls */
object BoundaryProgram:

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
      val n = params.gridSize
      val totalCells = n * n * n

      GIO.when(idx < totalCells):
        // Convert 1D index to 3D coordinates
        val z = idx / (n * n)
        val y = (idx / n).mod(n)
        val x = idx.mod(n)

        // Check if on boundary
        val onBoundary = (x === 0) || (x === n - 1) ||
                         (y === 0) || (y === n - 1) ||
                         (z === 0) || (z === n - 1)

        GIO.when(onBoundary):
          // No-slip: velocity = 0 at walls
          val boundaryVel = vec4(0.0f, 0.0f, 0.0f, 0.0f)
          GIO.write(state.velocity, idx, boundaryVel)
