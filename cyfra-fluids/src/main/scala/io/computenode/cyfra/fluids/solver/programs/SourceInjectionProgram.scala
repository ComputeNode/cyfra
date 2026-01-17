package io.computenode.cyfra.fluids.solver.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.FluidParams
import io.computenode.cyfra.fluids.solver.utils.GridUtils.*

/** Parameters for GPU-based scene source injection. */
case class SourceParams(
  frameIdx: Int32,
  discCenterX: Float32,
  discCenterY: Float32,
  discCenterZ: Float32,
  velocityRadius: Float32,
  velocityThickness: Float32,
  dyeRadius: Float32,
  dyeThickness: Float32,
  velocityX: Float32,
  velocityY: Float32,
  velocityZ: Float32,
  dyeValue: Float32,
  dyeFrameLimit: Int32,
) extends GStruct[SourceParams]

object SourceParams:
  /** Create source parameters from grid size and frame index. */
  def default(gridSize: Int, frameIdx: Int): SourceParams =
    val center = gridSize / 2.0f
    val currentRadius = gridSize * 0.15f
    val separation = gridSize * 0.35f
    val discThickness = gridSize * 0.05f
    val collisionSpeed = 2.0f
    val leftDiscX = center - separation

    SourceParams(
      frameIdx = frameIdx,
      discCenterX = leftDiscX,
      discCenterY = center,
      discCenterZ = center,
      velocityRadius = currentRadius * 3f,
      velocityThickness = discThickness * 100f,
      dyeRadius = currentRadius,
      dyeThickness = discThickness,
      velocityX = collisionSpeed,
      velocityY = 0.0f,
      velocityZ = 0.0f,
      dyeValue = 0.5f,
      dyeFrameLimit = 20,
    )

/** GPU layout for source injection program. */
case class SourceState(
  velocity: GBuffer[Vec4[Float32]],
  dye: GBuffer[Float32],
  fluidParams: GUniform[FluidParams],
  sourceParams: GUniform[SourceParams],
) derives Layout

/** GPU program for injecting velocity and dye sources into the fluid simulation.
  *
  * Creates disc-shaped source regions perpendicular to the X axis:
  *   - Velocity is injected every frame (added to existing velocity)
  *   - Dye is injected only for the first N frames (added to existing dye)
  */
object SourceInjectionProgram:

  def create: GProgram[Int, SourceState] =
    GProgram[Int, SourceState](
      layout = totalCells =>
        SourceState(
          velocity = GBuffer[Vec4[Float32]](totalCells),
          dye = GBuffer[Float32](totalCells),
          fluidParams = GUniform[FluidParams](),
          sourceParams = GUniform[SourceParams](),
        ),
      dispatch = (_, totalCells) => {
        val workgroupSize = 256
        val numWorkgroups = (totalCells + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): state =>
      val idx = GIO.invocationId
      val params = state.fluidParams.read
      val source = state.sourceParams.read
      val gridSize = params.gridSize
      val totalCells = gridSize * gridSize * gridSize

      GIO.when(idx < totalCells):
        val (x, y, z) = idxTo3D(idx, gridSize)

        // Distance from disc center in X direction
        val dx = x.asFloat - source.discCenterX
        val absDx = when(dx < 0.0f)(-dx).otherwise(dx)

        // Distance from disc center in YZ plane
        val dy = y.asFloat - source.discCenterY
        val dz = z.asFloat - source.discCenterZ
        val distYZ = sqrt(dy * dy + dz * dz)

        // Check if inside velocity source disc
        val insideVelDisc = (distYZ < source.velocityRadius) && (absDx < source.velocityThickness / 2.0f)

        // Compute velocity addition (0 if outside disc)
        val velAddX = when(insideVelDisc)(source.velocityX).otherwise(0.0f)
        val velAddY = when(insideVelDisc)(source.velocityY).otherwise(0.0f)
        val velAddZ = when(insideVelDisc)(source.velocityZ).otherwise(0.0f)

        val oldVel = GIO.read(state.velocity, idx)
        val newVel = vec4(oldVel.x + velAddX, oldVel.y + velAddY, oldVel.z + velAddZ, 0.0f)
        GIO.write(state.velocity, idx, newVel)

        // Check if inside dye source disc (only first N frames)
        val insideDyeDisc = (distYZ < source.dyeRadius) && (absDx < source.dyeThickness / 2.0f)
        val shouldInjectDye = source.frameIdx < source.dyeFrameLimit

        // Compute dye addition (0 if outside disc or past frame limit)
        val dyeAdd = when(shouldInjectDye && insideDyeDisc)(source.dyeValue).otherwise(0.0f)
        val oldDye = GIO.read(state.dye, idx)
        GIO.write(state.dye, idx, oldDye + dyeAdd)
