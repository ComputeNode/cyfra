package io.computenode.cyfra.fluids.solver.utils

import io.computenode.cyfra.dsl.{*, given}
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer

/** Utilities for creating and manipulating obstacle geometry in the fluid grid.
  * 
  * Obstacles are represented as a grid of Float32 values:
  * - 0 or negative: Fluid cell (no obstacle)
  * - Positive: Solid cell (obstacle), value encodes color/material (0.0-1.0 range)
  */
object ObstacleUtils:
  
  
  /** Check if a cell is a solid obstacle with bounds checking (returns false if out of bounds) */
  inline def isSolid(obstacles: GBuffer[Float32], idx: Int32, totalCells: Int32)(using io.computenode.cyfra.dsl.macros.Source): GBoolean =
    when(idx >= 0):
      when(idx < totalCells):
        obstacles.read(idx) > 0.0f
      .otherwise:
        false
    .otherwise:
      false
  
  /** Check if a 3D coordinate is inside a solid obstacle with bounds checking.
    * Returns false if coordinates are out of bounds.
    */
  inline def isSolidAt(obstacles: GBuffer[Float32], x: Int32, y: Int32, z: Int32, n: Int32)(using io.computenode.cyfra.dsl.macros.Source): GBoolean =
    when(GridUtils.inBounds(x, y, z, n)):
      isSolid(obstacles, GridUtils.coord3dToIdx(x, y, z, n), n * n * n)
    .otherwise:
      false
  
  /** Get obstacle color value (0.0-1.0, returns 0 if not solid).
    * Note: Does not perform bounds checking.
    */
  inline def getObstacleValue(obstacles: GBuffer[Float32], idx: Int32)(using io.computenode.cyfra.dsl.macros.Source): Float32 =
    max(0.0f, obstacles.read(idx))
  
  /** Get obstacle color value with bounds checking (returns 0 if out of bounds or not solid) */
  inline def getObstacleValueSafe(obstacles: GBuffer[Float32], idx: Int32, totalCells: Int32)(using io.computenode.cyfra.dsl.macros.Source): Float32 =
    when((idx >= 0) && (idx < totalCells)):
      max(0.0f, obstacles.read(idx))
    .otherwise:
      0.0f
  
  /** Compute surface normal for obstacle visualization (GPU side).
    * 
    * Uses central differences to estimate the gradient of the obstacle field.
    * Returns normalized vector pointing away from solid.
    */
  def computeNormal(
    obstacles: GBuffer[Float32],
    x: Int32,
    y: Int32,
    z: Int32,
    n: Int32
  )(using io.computenode.cyfra.dsl.macros.Source): Vec3[Float32] =
    
    val xp = when(isSolidAt(obstacles, x + 1, y, z, n))(1.0f).otherwise(0.0f)
    val xn = when(isSolidAt(obstacles, x - 1, y, z, n))(1.0f).otherwise(0.0f)
    val yp = when(isSolidAt(obstacles, x, y + 1, z, n))(1.0f).otherwise(0.0f)
    val yn = when(isSolidAt(obstacles, x, y - 1, z, n))(1.0f).otherwise(0.0f)
    val zp = when(isSolidAt(obstacles, x, y, z + 1, n))(1.0f).otherwise(0.0f)
    val zn = when(isSolidAt(obstacles, x, y, z - 1, n))(1.0f).otherwise(0.0f)
    
    val grad = vec3(xp - xn, yp - yn, zp - zn)
    
    normalize(grad)
