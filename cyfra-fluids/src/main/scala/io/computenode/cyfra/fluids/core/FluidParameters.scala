package io.computenode.cyfra.fluids.core

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct

/** Simulation parameters for 3D fluid dynamics.
  *
  * @param dt Time step size (seconds)
  * @param viscosity Kinematic viscosity (momentum diffusion coefficient)
  * @param diffusion Density diffusion coefficient
  * @param buoyancy Buoyancy force strength (how much hot fluid rises)
  * @param ambient Ambient temperature reference
  * @param gridSize Grid resolution (gridSize³ voxels)
  * @param iterationCount Number of Jacobi iterations for solvers
  */
case class FluidParams(
  dt: Float32,
  viscosity: Float32,
  diffusion: Float32,
  buoyancy: Float32,
  ambient: Float32,
  gridSize: Int32,
  iterationCount: Int32,
  windX: Float32,
  windZ: Float32
) extends GStruct[FluidParams]

object FluidParams:
  /** Default parameters for smoke simulation */
  def smoke(gridSize: Int): FluidParams =
    FluidParams(
      dt = 0.1f,
      viscosity = 0.00001f,
      diffusion = 0.001f,
      buoyancy = 0.5f,
      ambient = 0.0f,
      gridSize = gridSize,
      iterationCount = 20,
      windX = 0.0f,
      windZ = 0.0f
    )
  
  /** Parameters for thick, viscous fluid */
  def thickFluid(gridSize: Int): FluidParams =
    FluidParams(
      dt = 0.05f,
      viscosity = 0.01f,
      diffusion = 0.0001f,
      buoyancy = 0.0f,
      ambient = 0.0f,
      gridSize = gridSize,
      iterationCount = 30,
      windX = 0.0f,
      windZ = 0.0f
    )



