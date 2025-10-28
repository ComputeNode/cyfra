package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState, FluidStateDouble, GridUtils}
import GridUtils.*

/** Pressure projection programs for enforcing incompressibility */
object ProjectionProgram:

  /** Step 1: Compute divergence of velocity field */
  def divergence: GProgram[Int, FluidState] =
    GProgram[Int, FluidState](
      layout = totalCells => {
        import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
        FluidState(
          velocity = GBuffer[Vec3[Float32]](totalCells),
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

        // Fetch neighbors (pure operations)
        val velXP = readVec3Safe(state.velocity, x + 1, y, z, n)
        val velXM = readVec3Safe(state.velocity, x - 1, y, z, n)
        val velYP = readVec3Safe(state.velocity, x, y + 1, z, n)
        val velYM = readVec3Safe(state.velocity, x, y - 1, z, n)
        val velZP = readVec3Safe(state.velocity, x, y, z + 1, n)
        val velZM = readVec3Safe(state.velocity, x, y, z - 1, n)

        // Central difference approximation of divergence
        val dx = (velXP.x - velXM.x) * 0.5f
        val dy = (velYP.y - velYM.y) * 0.5f
        val dz = (velZP.z - velZM.z) * 0.5f

        val div = dx + dy + dz

        // Write result
        GIO.write(state.divergence, idx, div)

  /** Step 2: Solve Poisson equation for pressure via Jacobi iteration */
  def pressureSolve: GProgram[Int, FluidStateDouble] =
    GProgram[Int, FluidStateDouble](
      layout = totalCells => {
        import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
        FluidStateDouble(
          velocityCurrent = GBuffer[Vec3[Float32]](totalCells),
          pressureCurrent = GBuffer[Float32](totalCells),
          densityCurrent = GBuffer[Float32](totalCells),
          temperatureCurrent = GBuffer[Float32](totalCells),
          divergenceCurrent = GBuffer[Float32](totalCells),
          velocityPrevious = GBuffer[Vec3[Float32]](totalCells),
          pressurePrevious = GBuffer[Float32](totalCells),
          densityPrevious = GBuffer[Float32](totalCells),
          temperaturePrevious = GBuffer[Float32](totalCells),
          divergencePrevious = GBuffer[Float32](totalCells),
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

        // Fetch divergence (right-hand side) - pure operation
        val div = GIO.read(state.divergenceCurrent, idx)

        // Fetch neighbor pressures from previous buffer (pure operations)
        val pXM = readFloat32Safe(state.pressurePrevious, x - 1, y, z, n)
        val pXP = readFloat32Safe(state.pressurePrevious, x + 1, y, z, n)
        val pYM = readFloat32Safe(state.pressurePrevious, x, y - 1, z, n)
        val pYP = readFloat32Safe(state.pressurePrevious, x, y + 1, z, n)
        val pZM = readFloat32Safe(state.pressurePrevious, x, y, z - 1, n)
        val pZP = readFloat32Safe(state.pressurePrevious, x, y, z + 1, n)

        val neighborSum = pXM + pXP + pYM + pYP + pZM + pZP

        // Jacobi update: p = (Σneighbors - divergence) / 6
        val newPressure = (neighborSum - div) / 6.0f

        // Write result to current buffer
        GIO.write(state.pressureCurrent, idx, newPressure)

  /** Step 3: Subtract pressure gradient from velocity to make it divergence-free */
  def subtractGradient: GProgram[Int, FluidState] =
    GProgram[Int, FluidState](
      layout = totalCells => {
        import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
        FluidState(
          velocity = GBuffer[Vec3[Float32]](totalCells),
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

        // Read velocity (pure operation)
        val vel = GIO.read(state.velocity, idx)

        // Compute pressure gradient via central differences (pure operations)
        val pXP = readFloat32Safe(state.pressure, x + 1, y, z, n)
        val pXM = readFloat32Safe(state.pressure, x - 1, y, z, n)
        val pYP = readFloat32Safe(state.pressure, x, y + 1, z, n)
        val pYM = readFloat32Safe(state.pressure, x, y - 1, z, n)
        val pZP = readFloat32Safe(state.pressure, x, y, z + 1, n)
        val pZM = readFloat32Safe(state.pressure, x, y, z - 1, n)

        val gradPressure = vec3(
          (pXP - pXM) * 0.5f,
          (pYP - pYM) * 0.5f,
          (pZP - pZM) * 0.5f
        )

        // Subtract gradient to project onto divergence-free subspace
        val newVel = vel - gradPressure

        // Write result
        GIO.write(state.velocity, idx, newVel)
