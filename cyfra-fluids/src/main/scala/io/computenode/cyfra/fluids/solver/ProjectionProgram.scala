package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import GridUtils.*

/** Pressure projection programs for enforcing incompressibility */
object ProjectionProgram:

  /** Step 1: Compute divergence of velocity field */
  def divergence: GProgram[Int, FluidState] =
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
      val n = params.gridSize

      // Convert 1D index to 3D coordinates
      val z = idx / (n * n)
      val y = (idx / n).mod(n)
      val x = idx.mod(n)

      // Skip solid cells
      val isSolid = ObstacleUtils.isSolid(state.obstacles, idx, n)
      
      GIO.when(!isSolid):
        val velCenter = GIO.read(state.velocity, idx)
        
        // Check if neighbors are solid
        val solidXP = ObstacleUtils.isSolidAt(state.obstacles, x + 1, y, z, n)
        val solidXM = ObstacleUtils.isSolidAt(state.obstacles, x - 1, y, z, n)
        val solidYP = ObstacleUtils.isSolidAt(state.obstacles, x, y + 1, z, n)
        val solidYM = ObstacleUtils.isSolidAt(state.obstacles, x, y - 1, z, n)
        val solidZP = ObstacleUtils.isSolidAt(state.obstacles, x, y, z + 1, n)
        val solidZM = ObstacleUtils.isSolidAt(state.obstacles, x, y, z - 1, n)
        
        // Fetch fluid neighbors
        val velXP = readVec4Safe(state.velocity, x + 1, y, z, n)
        val velXM = readVec4Safe(state.velocity, x - 1, y, z, n)
        val velYP = readVec4Safe(state.velocity, x, y + 1, z, n)
        val velYM = readVec4Safe(state.velocity, x, y - 1, z, n)
        val velZP = readVec4Safe(state.velocity, x, y, z + 1, n)
        val velZM = readVec4Safe(state.velocity, x, y, z - 1, n)

        // Use one-sided differences where there are solid neighbors
        // At solid interface, normal velocity = 0 (no penetration)
        val dx = when(solidXP && solidXM)(
          0.0f  // Fully enclosed
        ).elseWhen(solidXP)(
          // Solid on +x: interface has u=0, so ∂u/∂x ≈ (0 - u_center) + (u_center - u_xm)/2
          -velCenter.x  // Simplified: normal component goes to zero
        ).elseWhen(solidXM)(
          // Solid on -x: interface has u=0, so ∂u/∂x ≈ (u_xp - u_center)/2 + (u_center - 0)
          velCenter.x  // Simplified
        ).otherwise(
          (velXP.x - velXM.x) * 0.5f  // Standard central difference
        )
        
        val dy = when(solidYP && solidYM)(
          0.0f
        ).elseWhen(solidYP)(
          -velCenter.y
        ).elseWhen(solidYM)(
          velCenter.y
        ).otherwise(
          (velYP.y - velYM.y) * 0.5f
        )
        
        val dz = when(solidZP && solidZM)(
          0.0f
        ).elseWhen(solidZP)(
          -velCenter.z
        ).elseWhen(solidZM)(
          velCenter.z
        ).otherwise(
          (velZP.z - velZM.z) * 0.5f
        )

        val div = dx + dy + dz

        // Write result
        GIO.write(state.divergence, idx, div)

  /** Step 2: Solve Poisson equation for pressure via Jacobi iteration */
  def pressureSolve: GProgram[Int, FluidStateDouble] =
    GProgram[Int, FluidStateDouble](
      layout = totalCells => {
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
          dyeCurrent = GBuffer[Float32](totalCells),
          dyePrevious = GBuffer[Float32](totalCells),
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

        // Check if this cell is solid
        val isSolid = ObstacleUtils.isSolid(state.obstacles, idx, n)
        
        // Only solve pressure in fluid cells
        GIO.when(!isSolid):
          // Fetch divergence (right-hand side) - pure operation
          val div = GIO.read(state.divergenceCurrent, idx)
          val pCenter = GIO.read(state.pressurePrevious, idx)

          // Check if neighbors are solid (for Neumann boundary condition)
          val solidXM = ObstacleUtils.isSolidAt(state.obstacles, x - 1, y, z, n)
          val solidXP = ObstacleUtils.isSolidAt(state.obstacles, x + 1, y, z, n)
          val solidYM = ObstacleUtils.isSolidAt(state.obstacles, x, y - 1, z, n)
          val solidYP = ObstacleUtils.isSolidAt(state.obstacles, x, y + 1, z, n)
          val solidZM = ObstacleUtils.isSolidAt(state.obstacles, x, y, z - 1, n)
          val solidZP = ObstacleUtils.isSolidAt(state.obstacles, x, y, z + 1, n)

          // Fetch neighbor pressures, use current pressure for solid neighbors (Neumann BC: ∂p/∂n = 0)
          val pXM = when(solidXM)(pCenter).otherwise(readFloat32Safe(state.pressurePrevious, x - 1, y, z, n))
          val pXP = when(solidXP)(pCenter).otherwise(readFloat32Safe(state.pressurePrevious, x + 1, y, z, n))
          val pYM = when(solidYM)(pCenter).otherwise(readFloat32Safe(state.pressurePrevious, x, y - 1, z, n))
          val pYP = when(solidYP)(pCenter).otherwise(readFloat32Safe(state.pressurePrevious, x, y + 1, z, n))
          val pZM = when(solidZM)(pCenter).otherwise(readFloat32Safe(state.pressurePrevious, x, y, z - 1, n))
          val pZP = when(solidZP)(pCenter).otherwise(readFloat32Safe(state.pressurePrevious, x, y, z + 1, n))

          val neighborSum = pXM + pXP + pYM + pYP + pZM + pZP

          // Jacobi update: p = (Σneighbors - divergence) / 6
          val newPressure = (neighborSum - div) / 6.0f

          // Write result to current buffer
          GIO.write(state.pressureCurrent, idx, newPressure)

  /** Step 3: Subtract pressure gradient from velocity to make it divergence-free */
  def subtractGradient: GProgram[Int, FluidState] =
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
      val n = params.gridSize
      val totalCells = n * n * n

      GIO.when(idx < totalCells):
        val (x, y, z) = idxTo3D(idx, n)

        // Skip solid cells
        val isSolid = ObstacleUtils.isSolid(state.obstacles, idx, n)
        
        GIO.when(!isSolid):
          // Read velocity and pressure (pure operations)
          val vel = GIO.read(state.velocity, idx)
          val pCenter = GIO.read(state.pressure, idx)

          // Check if neighbors are solid (for Neumann boundary condition)
          val solidXM = ObstacleUtils.isSolidAt(state.obstacles, x - 1, y, z, n)
          val solidXP = ObstacleUtils.isSolidAt(state.obstacles, x + 1, y, z, n)
          val solidYM = ObstacleUtils.isSolidAt(state.obstacles, x, y - 1, z, n)
          val solidYP = ObstacleUtils.isSolidAt(state.obstacles, x, y + 1, z, n)
          val solidZM = ObstacleUtils.isSolidAt(state.obstacles, x, y, z - 1, n)
          val solidZP = ObstacleUtils.isSolidAt(state.obstacles, x, y, z + 1, n)

          // Compute pressure gradient, use current pressure for solid neighbors (Neumann BC: ∂p/∂n = 0)
          val pXM = when(solidXM)(pCenter).otherwise(readFloat32Safe(state.pressure, x - 1, y, z, n))
          val pXP = when(solidXP)(pCenter).otherwise(readFloat32Safe(state.pressure, x + 1, y, z, n))
          val pYM = when(solidYM)(pCenter).otherwise(readFloat32Safe(state.pressure, x, y - 1, z, n))
          val pYP = when(solidYP)(pCenter).otherwise(readFloat32Safe(state.pressure, x, y + 1, z, n))
          val pZM = when(solidZM)(pCenter).otherwise(readFloat32Safe(state.pressure, x, y, z - 1, n))
          val pZP = when(solidZP)(pCenter).otherwise(readFloat32Safe(state.pressure, x, y, z + 1, n))

          val gradPressure = vec4(
            (pXP - pXM) * 0.5f,
            (pYP - pYM) * 0.5f,
            (pZP - pZM) * 0.5f,
            0.0f
          )

          // Subtract gradient to project onto divergence-free subspace
          val newVel = vel - gradPressure

          // Write result
          GIO.write(state.velocity, idx, newVel)
