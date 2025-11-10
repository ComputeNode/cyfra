package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.Value.{Float32, Int32, Vec3, Vec4}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}

/** GPU fluid state buffers (single-buffered) 
  * Note: Using Vec4 for velocity to ensure proper 16-byte alignment (std430)
  */
case class FluidState(
  velocity: GBuffer[Vec4[Float32]],     // 3D velocity field (w=0)
  pressure: GBuffer[Float32],           // Pressure field
  density: GBuffer[Float32],            // Density/smoke field
  temperature: GBuffer[Float32],        // Temperature field
  dye: GBuffer[Float32],                // Dye/tracer field (passive advection)
  divergence: GBuffer[Float32],         // Divergence scratch buffer
  obstacles: GBuffer[Float32],          // Obstacle field (<=0=fluid, >0=solid with color)
  params: GUniform[FluidParams]         // Simulation parameters
) extends Layout

/** Double-buffered state for read-while-write operations.
  * Layouts cannot be nested, so we flatten all buffers to top level.
  */
case class FluidStateDouble(
  // Current state buffers
  velocityCurrent: GBuffer[Vec4[Float32]],
  pressureCurrent: GBuffer[Float32],
  densityCurrent: GBuffer[Float32],
  temperatureCurrent: GBuffer[Float32],
  dyeCurrent: GBuffer[Float32],
  divergenceCurrent: GBuffer[Float32],
  
  // Previous state buffers (for ping-pong)
  velocityPrevious: GBuffer[Vec4[Float32]],
  pressurePrevious: GBuffer[Float32],
  densityPrevious: GBuffer[Float32],
  temperaturePrevious: GBuffer[Float32],
  dyePrevious: GBuffer[Float32],
  divergencePrevious: GBuffer[Float32],
  
  // Shared read-only buffers
  obstacles: GBuffer[Float32],          // Obstacle field (<=0=fluid, >0=solid with color)
  
  // Shared parameters
  params: GUniform[FluidParams]
) extends Layout
