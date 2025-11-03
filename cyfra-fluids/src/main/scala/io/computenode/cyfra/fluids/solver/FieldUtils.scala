package io.computenode.cyfra.fluids.solver

import org.lwjgl.BufferUtils

import java.nio.ByteBuffer

object FieldUtils:

  /** Create empty field buffer initialized with zeros.
   *
   * @param gridSize Grid resolution
   * @return ByteBuffer containing Float32 values, one per grid cell
   */
  def createEmpty(gridSize: Int): ByteBuffer =
    val totalCells = gridSize * gridSize * gridSize
    val buffer = BufferUtils.createByteBuffer(totalCells * 4)

    for i <- 0 until totalCells do
      buffer.putFloat(0.0f)

    buffer.flip()
    buffer

  /** Set field values to a constant within a spherical region.
   *
   * @param field ByteBuffer to modify (must be rewound after this call)
   * @param gridSize Grid resolution
   * @param centerX Sphere center X coordinate
   * @param centerY Sphere center Y coordinate
   * @param centerZ Sphere center Z coordinate
   * @param radius Sphere radius in grid cells
   * @param value Field value to set (default 1.0)
   */
  def addSphere(
    field: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    radius: Float,
    value: Float = 1.0f
  ): Unit =
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          // Calculate distance from cell center to sphere center
          val dx = x.toFloat - centerX
          val dy = y.toFloat - centerY
          val dz = z.toFloat - centerZ
          val dist = Math.sqrt(dx*dx + dy*dy + dz*dz).toFloat

          // Set value if inside sphere
          if dist <= radius then
            field.putFloat(value)

    field.position(gridSize *  gridSize * gridSize * 4)
    field.rewind()

  /** Set field values to a constant within a box region.
   *
   * @param field ByteBuffer to modify
   * @param gridSize Grid resolution
   * @param minX Minimum X coordinate (inclusive)
   * @param maxX Maximum X coordinate (inclusive)
   * @param minY Minimum Y coordinate (inclusive)
   * @param maxY Maximum Y coordinate (inclusive)
   * @param minZ Minimum Z coordinate (inclusive)
   * @param maxZ Maximum Z coordinate (inclusive)
   * @param value Field value to set (default 1.0)
   */
  def addBox(
    field: ByteBuffer,
    gridSize: Int,
    minX: Int,
    maxX: Int,
    minY: Int,
    maxY: Int,
    minZ: Int,
    maxZ: Int,
    value: Float = 1.0f
  ): Unit =
    field.rewind()

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          // Check if inside box
          if x >= minX && x <= maxX &&
            y >= minY && y <= maxY &&
            z >= minZ && z <= maxZ then
            field.putFloat(value)

    field.rewind()

  /** Set field values to a constant within a cylindrical region (aligned with Y axis).
   *
   * @param field ByteBuffer to modify
   * @param gridSize Grid resolution
   * @param centerX Cylinder center X coordinate
   * @param centerZ Cylinder center Z coordinate
   * @param radius Cylinder radius in grid cells
   * @param minY Minimum Y coordinate (inclusive)
   * @param maxY Maximum Y coordinate (inclusive)
   * @param value Field value to set (default 1.0)
   */
  def addCylinder(
    field: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerZ: Float,
    radius: Float,
    minY: Int,
    maxY: Int,
    value: Float = 1.0f
  ): Unit =
    field.rewind()

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          // Check if inside cylinder
          if y >= minY && y <= maxY then
            val dx = x.toFloat + 0.5f - centerX
            val dz = z.toFloat + 0.5f - centerZ
            val dist = Math.sqrt(dx*dx + dz*dz).toFloat

            if dist <= radius then
              field.putFloat(value)

    field.rewind()

  /** Set field values to a constant in a boundary region around the grid perimeter.
   *
   * @param field ByteBuffer to modify
   * @param gridSize Grid resolution
   * @param thickness Boundary thickness in cells
   * @param value Field value to set (default 1.0)
   */
  def addWalls(
    field: ByteBuffer,
    gridSize: Int,
    thickness: Int = 1,
    value: Float = 1.0f
  ): Unit =
    field.rewind()

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          // Set value if near any boundary
          if x < thickness || x >= gridSize - thickness ||
            y < thickness || y >= gridSize - thickness ||
            z < thickness || z >= gridSize - thickness then
            field.putFloat(value)

    field.rewind()