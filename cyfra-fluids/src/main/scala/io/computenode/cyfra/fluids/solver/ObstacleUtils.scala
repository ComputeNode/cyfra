package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GBuffer
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
    import io.computenode.cyfra.dsl.control.When.when
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
    import io.computenode.cyfra.dsl.control.When.when
    when(GridUtils.inBounds(x, y, z, n)):
      isSolid(obstacles, GridUtils.coord3dToIdx(x, y, z, n), n * n * n)
    .otherwise:
      false
  
  /** Get obstacle color value (0.0-1.0, returns 0 if not solid).
    * Note: Does not perform bounds checking.
    */
  inline def getObstacleValue(obstacles: GBuffer[Float32], idx: Int32)(using io.computenode.cyfra.dsl.macros.Source): Float32 =
    import io.computenode.cyfra.dsl.library.Functions.max
    max(0.0f, obstacles.read(idx))
  
  /** Get obstacle color value with bounds checking (returns 0 if out of bounds or not solid) */
  inline def getObstacleValueSafe(obstacles: GBuffer[Float32], idx: Int32, totalCells: Int32)(using io.computenode.cyfra.dsl.macros.Source): Float32 =
    import io.computenode.cyfra.dsl.library.Functions.max
    import io.computenode.cyfra.dsl.control.When.when
    when((idx >= 0) && (idx < totalCells)):
      max(0.0f, obstacles.read(idx))
    .otherwise:
      0.0f
  
  /** Create empty obstacle buffer (all cells are fluid) */
  def createEmpty(gridSize: Int): ByteBuffer =
    val totalCells = gridSize * gridSize * gridSize
    val buffer = BufferUtils.createByteBuffer(totalCells * 4)
    
    for i <- 0 until totalCells do
      buffer.putFloat(0.0f) // 0 = fluid

    buffer.flip()
    buffer
  
  /** Add a sphere obstacle to the buffer.
    * 
    * @param obstacles ByteBuffer to modify (must be rewound after this call)
    * @param gridSize Grid resolution
    * @param centerX Sphere center X coordinate
    * @param centerY Sphere center Y coordinate
    * @param centerZ Sphere center Z coordinate
    * @param radius Sphere radius in grid cells
    * @param color Obstacle color/brightness value (0.0-1.0, default 1.0)
    */
  def addSphere(
    obstacles: ByteBuffer, 
    gridSize: Int,
    centerX: Float, 
    centerY: Float, 
    centerZ: Float, 
    radius: Float,
    color: Float = 1.0f
  ): Unit =
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          obstacles.position(idx * 4)
          
          // Calculate distance from cell center to sphere center
          val dx = x.toFloat - centerX
          val dy = y.toFloat - centerY
          val dz = z.toFloat - centerZ
          val dist = Math.sqrt(dx*dx + dy*dy + dz*dz).toFloat
          
          // Mark as solid if inside sphere
          if dist <= radius then
            obstacles.putFloat(color) // Solid with color value

    obstacles.position(gridSize *  gridSize * gridSize * 4)
    obstacles.rewind()
  
  /** Add a box obstacle to the buffer.
    * 
    * @param obstacles ByteBuffer to modify
    * @param gridSize Grid resolution
    * @param minX Minimum X coordinate (inclusive)
    * @param maxX Maximum X coordinate (inclusive)
    * @param minY Minimum Y coordinate (inclusive)
    * @param maxY Maximum Y coordinate (inclusive)
    * @param minZ Minimum Z coordinate (inclusive)
    * @param maxZ Maximum Z coordinate (inclusive)
    * @param color Obstacle color/brightness value (0.0-1.0, default 1.0)
    */
  def addBox(
    obstacles: ByteBuffer,
    gridSize: Int,
    minX: Int,
    maxX: Int,
    minY: Int,
    maxY: Int,
    minZ: Int,
    maxZ: Int,
    color: Float = 1.0f
  ): Unit =
    obstacles.rewind()
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          obstacles.position(idx * 4)
          
          // Check if inside box
          if x >= minX && x <= maxX && 
             y >= minY && y <= maxY && 
             z >= minZ && z <= maxZ then
            obstacles.putFloat(color) // Solid with color value
    
    obstacles.rewind()
  
  /** Add a cylinder obstacle to the buffer (aligned with Y axis).
    * 
    * @param obstacles ByteBuffer to modify
    * @param gridSize Grid resolution
    * @param centerX Cylinder center X coordinate
    * @param centerZ Cylinder center Z coordinate
    * @param radius Cylinder radius in grid cells
    * @param minY Minimum Y coordinate (inclusive)
    * @param maxY Maximum Y coordinate (inclusive)
    * @param color Obstacle color/brightness value (0.0-1.0, default 1.0)
    */
  def addCylinder(
    obstacles: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerZ: Float,
    radius: Float,
    minY: Int,
    maxY: Int,
    color: Float = 1.0f
  ): Unit =
    obstacles.rewind()
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          obstacles.position(idx * 4)
          
          // Check if inside cylinder
          if y >= minY && y <= maxY then
            val dx = x.toFloat + 0.5f - centerX
            val dz = z.toFloat + 0.5f - centerZ
            val dist = Math.sqrt(dx*dx + dz*dz).toFloat
            
            if dist <= radius then
              obstacles.putFloat(color) // Solid with color value
    
    obstacles.rewind()
  
  /** Add walls around the perimeter of the grid.
    * 
    * @param obstacles ByteBuffer to modify
    * @param gridSize Grid resolution
    * @param thickness Wall thickness in cells
    * @param color Obstacle color/brightness value (0.0-1.0, default 1.0)
    */
  def addWalls(
    obstacles: ByteBuffer,
    gridSize: Int,
    thickness: Int = 1,
    color: Float = 1.0f
  ): Unit =
    obstacles.rewind()
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          obstacles.position(idx * 4)
          
          // Mark as solid if near any boundary
          if x < thickness || x >= gridSize - thickness ||
             y < thickness || y >= gridSize - thickness ||
             z < thickness || z >= gridSize - thickness then
            obstacles.putFloat(color) // Solid with color value
    
    obstacles.rewind()
  
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
    import io.computenode.cyfra.dsl.library.Math3D.*
    import io.computenode.cyfra.dsl.control.When.when
    
    // Sample neighboring cells using when/otherwise for GPU compatibility
    val xp = when(isSolidAt(obstacles, x + 1, y, z, n))(1.0f).otherwise(0.0f)
    val xn = when(isSolidAt(obstacles, x - 1, y, z, n))(1.0f).otherwise(0.0f)
    val yp = when(isSolidAt(obstacles, x, y + 1, z, n))(1.0f).otherwise(0.0f)
    val yn = when(isSolidAt(obstacles, x, y - 1, z, n))(1.0f).otherwise(0.0f)
    val zp = when(isSolidAt(obstacles, x, y, z + 1, n))(1.0f).otherwise(0.0f)
    val zn = when(isSolidAt(obstacles, x, y, z - 1, n))(1.0f).otherwise(0.0f)
    
    // Gradient points from low (0) to high (1) density
    val grad = vec3(xp - xn, yp - yn, zp - zn)
    
    // Normalize (returns zero vector if no gradient)
    normalize(grad)

