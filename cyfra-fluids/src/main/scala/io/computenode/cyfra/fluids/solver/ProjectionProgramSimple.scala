package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import GridUtils.*

/** Simplified pressure projection matching GPU Gems Chapter 38.
  * No obstacle handling - assumes open domain with simple boundaries.
  */
object ProjectionProgramSimple:

  /** Step 1: Compute divergence of velocity field (no obstacle handling) */
  def divergence: GProgram[Int, FluidState] =
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
        
        // Fetch neighbors - simple, no obstacle checks
        val velXP = readVec4Safe(state.velocity, x + 1, y, z, n)
        val velXM = readVec4Safe(state.velocity, x - 1, y, z, n)
        val velYP = readVec4Safe(state.velocity, x, y + 1, z, n)
        val velYM = readVec4Safe(state.velocity, x, y - 1, z, n)
        val velZP = readVec4Safe(state.velocity, x, y, z + 1, n)
        val velZM = readVec4Safe(state.velocity, x, y, z - 1, n)

        // Central differences: ∇·u = ∂u/∂x + ∂v/∂y + ∂w/∂z
        val dx = (velXP.x - velXM.x) * 0.5f
        val dy = (velYP.y - velYM.y) * 0.5f
        val dz = (velZP.z - velZM.z) * 0.5f
        val div = dx + dy + dz

        GIO.write(state.divergence, idx, div)

  /** Step 2: Solve Poisson equation for pressure via Jacobi iteration (no obstacles) */
  def pressureSolve: GProgram[Int, FluidStateDouble] =
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
        val (x, y, z) = idxTo3D(idx, n)

        // Fetch divergence (RHS of Poisson equation)
        val div = GIO.read(state.divergenceCurrent, idx)

        // Fetch neighbor pressures - simple, no obstacle checks
        val pXM = readFloat32Safe(state.pressurePrevious, x - 1, y, z, n)
        val pXP = readFloat32Safe(state.pressurePrevious, x + 1, y, z, n)
        val pYM = readFloat32Safe(state.pressurePrevious, x, y - 1, z, n)
        val pYP = readFloat32Safe(state.pressurePrevious, x, y + 1, z, n)
        val pZM = readFloat32Safe(state.pressurePrevious, x, y, z - 1, n)
        val pZP = readFloat32Safe(state.pressurePrevious, x, y, z + 1, n)

        val neighborSum = pXM + pXP + pYM + pYP + pZM + pZP

        // Jacobi update: p_new = (Σneighbors - divergence) / 6
        // This solves ∇²p = ∇·u
        val newPressure = (neighborSum - div) / 6.0f

        GIO.write(state.pressureCurrent, idx, newPressure)

  /** Step 3: Subtract pressure gradient from velocity (no obstacles) */
  def subtractGradient: GProgram[Int, FluidState] =
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

        // Read current velocity
        val vel = GIO.read(state.velocity, idx)

        // Compute pressure gradient via central differences
        val pXM = readFloat32Safe(state.pressure, x - 1, y, z, n)
        val pXP = readFloat32Safe(state.pressure, x + 1, y, z, n)
        val pYM = readFloat32Safe(state.pressure, x, y - 1, z, n)
        val pYP = readFloat32Safe(state.pressure, x, y + 1, z, n)
        val pZM = readFloat32Safe(state.pressure, x, y, z - 1, n)
        val pZP = readFloat32Safe(state.pressure, x, y, z + 1, n)

        val gradPressure = vec4(
          (pXP - pXM) * 0.5f,
          (pYP - pYM) * 0.5f,
          (pZP - pZM) * 0.5f,
          0.0f
        )

        // Project velocity: u_new = u - ∇p
        // This enforces incompressibility (∇·u = 0)
        val newVel = vel - gradPressure

        GIO.write(state.velocity, idx, newVel)



