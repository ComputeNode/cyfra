package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty

/** Vorticity Confinement Program
  * 
  * Adds vorticity confinement forces to restore small-scale rolling features
  * that are lost due to numerical dissipation.
  * 
  * Based on: Fedkiw et al. "Visual Simulation of Smoke" (SIGGRAPH 2001)
  * and GPU Gems Chapter 38: "Fast Fluid Dynamics Simulation on the GPU"
  * 
  * The algorithm:
  * 1. Compute vorticity (curl): ω = ∇ × u
  * 2. Compute gradient of vorticity magnitude: ∇|ω|
  * 3. Normalize: N = ∇|ω| / |∇|ω||
  * 4. Apply confinement force: f_conf = ε(N × ω)Δt
  */
object VorticityConfinementProgram:

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
      import io.computenode.cyfra.dsl.library.Functions.{sqrt, max}
      
      val idx = GIO.invocationId
      val params = state.params.read
      val n = params.gridSize
      val totalCells = n * n * n
      
      GIO.when(idx < totalCells):
        val (x, y, z) = GridUtils.idxTo3D(idx, n)
        
        // Skip boundaries (vorticity confinement applied only to interior cells)
        val isInterior = (x > 0) && (x < n - 1) &&
                        (y > 0) && (y < n - 1) &&
                        (z > 0) && (z < n - 1)
        
        GIO.when(isInterior):
          // Read neighbor velocities (center differences)
          val velXP = GridUtils.readVec4Safe(state.velocity, x + 1, y, z, n)
          val velXM = GridUtils.readVec4Safe(state.velocity, x - 1, y, z, n)
          val velYP = GridUtils.readVec4Safe(state.velocity, x, y + 1, z, n)
          val velYM = GridUtils.readVec4Safe(state.velocity, x, y - 1, z, n)
          val velZP = GridUtils.readVec4Safe(state.velocity, x, y, z + 1, n)
          val velZM = GridUtils.readVec4Safe(state.velocity, x, y, z - 1, n)
          
          // Compute vorticity (curl): ω = ∇ × u
          // ω_x = ∂w/∂y - ∂v/∂z
          // ω_y = ∂u/∂z - ∂w/∂x
          // ω_z = ∂v/∂x - ∂u/∂y
          val dx = 1.0f  // Grid spacing
          val omegaX = (velYP.z - velYM.z) / (2.0f * dx) - (velZP.y - velZM.y) / (2.0f * dx)
          val omegaY = (velZP.x - velZM.x) / (2.0f * dx) - (velXP.z - velXM.z) / (2.0f * dx)
          val omegaZ = (velXP.y - velXM.y) / (2.0f * dx) - (velYP.x - velYM.x) / (2.0f * dx)
          val omega = vec3(omegaX, omegaY, omegaZ)
          
          // Compute vorticity magnitude at neighboring cells
          // For (x+1, y, z)
          val velXP_YP = GridUtils.readVec4Safe(state.velocity, x + 1, y + 1, z, n)
          val velXP_YM = GridUtils.readVec4Safe(state.velocity, x + 1, y - 1, z, n)
          val velXP_ZP = GridUtils.readVec4Safe(state.velocity, x + 1, y, z + 1, n)
          val velXP_ZM = GridUtils.readVec4Safe(state.velocity, x + 1, y, z - 1, n)
          val omegaXP_x = (velXP_YP.z - velXP_YM.z) / (2.0f * dx) - (velXP_ZP.y - velXP_ZM.y) / (2.0f * dx)
          val omegaXP_y = (velXP_ZP.x - velXP_ZM.x) / (2.0f * dx) - (velYP.z - velYM.z) / (2.0f * dx)
          val omegaXP_z = (velYP.y - velYM.y) / (2.0f * dx) - (velXP_YP.x - velXP_YM.x) / (2.0f * dx)
          val magXP = sqrt(omegaXP_x * omegaXP_x + omegaXP_y * omegaXP_y + omegaXP_z * omegaXP_z)
          
          // For (x-1, y, z)
          val velXM_YP = GridUtils.readVec4Safe(state.velocity, x - 1, y + 1, z, n)
          val velXM_YM = GridUtils.readVec4Safe(state.velocity, x - 1, y - 1, z, n)
          val velXM_ZP = GridUtils.readVec4Safe(state.velocity, x - 1, y, z + 1, n)
          val velXM_ZM = GridUtils.readVec4Safe(state.velocity, x - 1, y, z - 1, n)
          val omegaXM_x = (velXM_YP.z - velXM_YM.z) / (2.0f * dx) - (velXM_ZP.y - velXM_ZM.y) / (2.0f * dx)
          val omegaXM_y = (velXM_ZP.x - velXM_ZM.x) / (2.0f * dx) - (velYM.z - velYP.z) / (2.0f * dx)
          val omegaXM_z = (velYM.y - velYP.y) / (2.0f * dx) - (velXM_YP.x - velXM_YM.x) / (2.0f * dx)
          val magXM = sqrt(omegaXM_x * omegaXM_x + omegaXM_y * omegaXM_y + omegaXM_z * omegaXM_z)
          
          // For (x, y+1, z)
          val velYP_XP = GridUtils.readVec4Safe(state.velocity, x + 1, y + 1, z, n)
          val velYP_XM = GridUtils.readVec4Safe(state.velocity, x - 1, y + 1, z, n)
          val velYP_ZP = GridUtils.readVec4Safe(state.velocity, x, y + 1, z + 1, n)
          val velYP_ZM = GridUtils.readVec4Safe(state.velocity, x, y + 1, z - 1, n)
          val omegaYP_x = (velYP_ZP.z - velYP_ZM.z) / (2.0f * dx) - (velZP.y - velZM.y) / (2.0f * dx)
          val omegaYP_y = (velZP.x - velZM.x) / (2.0f * dx) - (velYP_XP.z - velYP_XM.z) / (2.0f * dx)
          val omegaYP_z = (velYP_XP.y - velYP_XM.y) / (2.0f * dx) - (velXP.x - velXM.x) / (2.0f * dx)
          val magYP = sqrt(omegaYP_x * omegaYP_x + omegaYP_y * omegaYP_y + omegaYP_z * omegaYP_z)
          
          // For (x, y-1, z)
          val velYM_XP = GridUtils.readVec4Safe(state.velocity, x + 1, y - 1, z, n)
          val velYM_XM = GridUtils.readVec4Safe(state.velocity, x - 1, y - 1, z, n)
          val velYM_ZP = GridUtils.readVec4Safe(state.velocity, x, y - 1, z + 1, n)
          val velYM_ZM = GridUtils.readVec4Safe(state.velocity, x, y - 1, z - 1, n)
          val omegaYM_x = (velYM_ZP.z - velYM_ZM.z) / (2.0f * dx) - (velZM.y - velZP.y) / (2.0f * dx)
          val omegaYM_y = (velZM.x - velZP.x) / (2.0f * dx) - (velYM_XP.z - velYM_XM.z) / (2.0f * dx)
          val omegaYM_z = (velYM_XP.y - velYM_XM.y) / (2.0f * dx) - (velXM.x - velXP.x) / (2.0f * dx)
          val magYM = sqrt(omegaYM_x * omegaYM_x + omegaYM_y * omegaYM_y + omegaYM_z * omegaYM_z)
          
          // For (x, y, z+1)
          val velZP_XP = GridUtils.readVec4Safe(state.velocity, x + 1, y, z + 1, n)
          val velZP_XM = GridUtils.readVec4Safe(state.velocity, x - 1, y, z + 1, n)
          val velZP_YP = GridUtils.readVec4Safe(state.velocity, x, y + 1, z + 1, n)
          val velZP_YM = GridUtils.readVec4Safe(state.velocity, x, y - 1, z + 1, n)
          val omegaZP_x = (velZP_YP.z - velZP_YM.z) / (2.0f * dx) - (velYP.y - velYM.y) / (2.0f * dx)
          val omegaZP_y = (velXP.z - velXM.z) / (2.0f * dx) - (velZP_XP.z - velZP_XM.z) / (2.0f * dx)
          val omegaZP_z = (velZP_XP.y - velZP_XM.y) / (2.0f * dx) - (velZP_YP.x - velZP_YM.x) / (2.0f * dx)
          val magZP = sqrt(omegaZP_x * omegaZP_x + omegaZP_y * omegaZP_y + omegaZP_z * omegaZP_z)
          
          // For (x, y, z-1)
          val velZM_XP = GridUtils.readVec4Safe(state.velocity, x + 1, y, z - 1, n)
          val velZM_XM = GridUtils.readVec4Safe(state.velocity, x - 1, y, z - 1, n)
          val velZM_YP = GridUtils.readVec4Safe(state.velocity, x, y + 1, z - 1, n)
          val velZM_YM = GridUtils.readVec4Safe(state.velocity, x, y - 1, z - 1, n)
          val omegaZM_x = (velZM_YP.z - velZM_YM.z) / (2.0f * dx) - (velYM.y - velYP.y) / (2.0f * dx)
          val omegaZM_y = (velXM.z - velXP.z) / (2.0f * dx) - (velZM_XP.z - velZM_XM.z) / (2.0f * dx)
          val omegaZM_z = (velZM_XP.y - velZM_XM.y) / (2.0f * dx) - (velZM_YP.x - velZM_YM.x) / (2.0f * dx)
          val magZM = sqrt(omegaZM_x * omegaZM_x + omegaZM_y * omegaZM_y + omegaZM_z * omegaZM_z)
          
          // Compute gradient of vorticity magnitude: ∇|ω|
          val gradMagX = (magXP - magXM) / (2.0f * dx)
          val gradMagY = (magYP - magYM) / (2.0f * dx)
          val gradMagZ = (magZP - magZM) / (2.0f * dx)
          val gradMag = vec3(gradMagX, gradMagY, gradMagZ)
          
          // Normalize: N = ∇|ω| / |∇|ω||
          val gradMagLength = sqrt(gradMagX * gradMagX + gradMagY * gradMagY + gradMagZ * gradMagZ)
          val epsilon = 1e-6f  // Prevent division by zero
          val N = when(gradMagLength > epsilon):
            vec3(
              gradMagX / gradMagLength,
              gradMagY / gradMagLength,
              gradMagZ / gradMagLength
            )
          .otherwise:
            vec3(0.0f, 0.0f, 0.0f)
          
          // Compute vorticity confinement force: f_conf = ε(N × ω)
          // Cross product: N × ω
          val forceX = N.y * omega.z - N.z * omega.y
          val forceY = N.z * omega.x - N.x * omega.z
          val forceZ = N.x * omega.y - N.y * omega.x
          
          // Vorticity confinement coefficient (adjustable parameter)
          // Typical values: 0.1 to 0.5 for smoke simulation
          val vorticityEpsilon = 0.3f
          
          val confinementForce = vec3(
            forceX * vorticityEpsilon,
            forceY * vorticityEpsilon,
            forceZ * vorticityEpsilon
          )
          
          // Apply force to velocity
          val vel = state.velocity.read(idx)
          val newVel = vec4(
            vel.x + confinementForce.x * params.dt,
            vel.y + confinementForce.y * params.dt,
            vel.z + confinementForce.z * params.dt,
            0.0f
          )
          
          GIO.write(state.velocity, idx, newVel)

