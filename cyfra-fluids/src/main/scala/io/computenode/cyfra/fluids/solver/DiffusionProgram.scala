package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import GridUtils.*

/** Implements diffusion via Jacobi iteration.
  * Solves: (I - ν·Δt·∇²)v_new = v_old
  */
object DiffusionProgram:

  def create: GProgram[Int, FluidStateDouble] =
    GProgram[Int, FluidStateDouble](
      layout = totalCells => {
        import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
        FluidStateDouble(
          velocityCurrent = GBuffer[Vec4[Float32]](totalCells),
          pressureCurrent = GBuffer[Float32](totalCells),
          densityCurrent = GBuffer[Float32](totalCells),
          temperatureCurrent = GBuffer[Float32](totalCells),
          divergenceCurrent = GBuffer[Float32](totalCells),
          velocityPrevious = GBuffer[Vec4[Float32]](totalCells),
          pressurePrevious = GBuffer[Float32](totalCells),
          densityPrevious = GBuffer[Float32](totalCells),
          temperaturePrevious = GBuffer[Float32](totalCells),
          divergencePrevious = GBuffer[Float32](totalCells),
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
        // Convert 1D index to 3D coordinates
        val (x, y, z) = idxTo3D(idx, n)

        // Jacobi iteration coefficients
        val alpha = 1.0f / (params.viscosity * params.dt)
        val beta = 1.0f / (6.0f + alpha)

        // Read center value from previous buffer (pure operation)
        val center = GIO.read(state.velocityPrevious, idx)

        // Sample six neighbors from previous buffer (pure operations)
        val xm = readVec3Safe(state.velocityPrevious, x - 1, y, z, n)
        val xp = readVec3Safe(state.velocityPrevious, x + 1, y, z, n)
        val ym = readVec3Safe(state.velocityPrevious, x, y - 1, z, n)
        val yp = readVec3Safe(state.velocityPrevious, x, y + 1, z, n)
        val zm = readVec3Safe(state.velocityPrevious, x, y, z - 1, n)
        val zp = readVec3Safe(state.velocityPrevious, x, y, z + 1, n)

        val neighborSum = xm + xp + ym + yp + zm + zp

        // Jacobi update: x = (b + Σneighbors) / (diagonal coeff + 6)
        val newVel = (center * alpha + neighborSum) * beta

        // Write result to current buffer
        GIO.write(state.velocityCurrent, idx, newVel)
