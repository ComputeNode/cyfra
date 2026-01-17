package io.computenode.cyfra.fluids.solver.utils

import org.lwjgl.BufferUtils

import java.nio.ByteBuffer

/** Utilities for initializing and manipulating fluid field buffers on the CPU side. */
object FieldUtils:

  /** Create empty field buffer initialized with zeros.
    *
    * @param gridSize
    *   Grid resolution
    * @return
    *   ByteBuffer containing Float32 values, one per grid cell
    */
  def createEmpty(gridSize: Int): ByteBuffer =
    val totalCells = gridSize * gridSize * gridSize
    val buffer = BufferUtils.createByteBuffer(totalCells * 4)

    for i <- 0 until totalCells do buffer.putFloat(0.0f)

    buffer.flip()
    buffer

  /** Set field values to a constant within a spherical region.
    *
    * @param field
    *   ByteBuffer to modify (must be rewound after this call)
    * @param gridSize
    *   Grid resolution
    * @param centerX
    *   Sphere center X coordinate
    * @param centerY
    *   Sphere center Y coordinate
    * @param centerZ
    *   Sphere center Z coordinate
    * @param radius
    *   Sphere radius in grid cells
    * @param value
    *   Field value to set (default 1.0)
    */
  def addSphere(field: ByteBuffer, gridSize: Int, centerX: Float, centerY: Float, centerZ: Float, radius: Float, value: Float = 1.0f): Unit =
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          val dx = x.toFloat - centerX
          val dy = y.toFloat - centerY
          val dz = z.toFloat - centerZ
          val dist = Math.sqrt(dx * dx + dy * dy + dz * dz).toFloat

          if dist <= radius then field.putFloat(value)

    field.position(gridSize * gridSize * gridSize * 4)
    field.rewind()

  /** Set field values to a constant within a box region.
    *
    * @param field
    *   ByteBuffer to modify
    * @param gridSize
    *   Grid resolution
    * @param minX
    *   Minimum X coordinate (inclusive)
    * @param maxX
    *   Maximum X coordinate (inclusive)
    * @param minY
    *   Minimum Y coordinate (inclusive)
    * @param maxY
    *   Maximum Y coordinate (inclusive)
    * @param minZ
    *   Minimum Z coordinate (inclusive)
    * @param maxZ
    *   Maximum Z coordinate (inclusive)
    * @param value
    *   Field value to set (default 1.0)
    */
  def addBox(field: ByteBuffer, gridSize: Int, minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int, value: Float = 1.0f): Unit =
    field.rewind()

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          if x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ then field.putFloat(value)

    field.rewind()

  /** Set field values to a constant within a cylindrical region (aligned with Y axis).
    *
    * @param field
    *   ByteBuffer to modify
    * @param gridSize
    *   Grid resolution
    * @param centerX
    *   Cylinder center X coordinate
    * @param centerZ
    *   Cylinder center Z coordinate
    * @param radius
    *   Cylinder radius in grid cells
    * @param minY
    *   Minimum Y coordinate (inclusive)
    * @param maxY
    *   Maximum Y coordinate (inclusive)
    * @param value
    *   Field value to set (default 1.0)
    */
  def addCylinder(field: ByteBuffer, gridSize: Int, centerX: Float, centerZ: Float, radius: Float, minY: Int, maxY: Int, value: Float = 1.0f): Unit =
    field.rewind()

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          if y >= minY && y <= maxY then
            val dx = x.toFloat + 0.5f - centerX
            val dz = z.toFloat + 0.5f - centerZ
            val dist = Math.sqrt(dx * dx + dz * dz).toFloat

            if dist <= radius then field.putFloat(value)

    field.rewind()

  /** Set field values to a constant in a boundary region around the grid perimeter.
    *
    * @param field
    *   ByteBuffer to modify
    * @param gridSize
    *   Grid resolution
    * @param thickness
    *   Boundary thickness in cells
    * @param value
    *   Field value to set (default 1.0)
    */
  def addWalls(field: ByteBuffer, gridSize: Int, thickness: Int = 1, value: Float = 1.0f): Unit =
    field.rewind()

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          field.position(idx * 4)

          if x < thickness || x >= gridSize - thickness || y < thickness || y >= gridSize - thickness || z < thickness || z >= gridSize - thickness
          then field.putFloat(value)

    field.rewind()

  /** Add a spiral obstacle pattern inspired by Scala logo.
    *
    * Creates a 3D ribbon-shaped spiral with CONSTANT radius.
    *
    * RIBBON SHAPE means:
    *   - WIDE along the curve (tangent direction)
    *   - THIN perpendicular to the curve in horizontal plane (radial direction)
    *   - Has VERTICAL HEIGHT
    *
    * @param obstacles
    *   Obstacle buffer to modify
    * @param gridSize
    *   Grid resolution
    * @param centerX
    *   Center X position
    * @param centerY
    *   Center Y position
    * @param centerZ
    *   Center Z position
    * @param spiralRadius
    *   Constant radius of the spiral
    * @param spiralHeight
    *   Total height of the spiral
    * @param numTurns
    *   Number of complete rotations
    * @param ribbonWidth
    *   Width along the curve (tangent)
    * @param ribbonThickness
    *   Thickness perpendicular to curve (radial - thin!)
    * @param ribbonHeight
    *   Vertical height of the ribbon
    * @param rotationDegrees
    *   Rotation around Y axis in degrees
    * @param value
    *   Obstacle value (typically 1.0 for solid)
    */
  def addSpiralObstacle(
    obstacles: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    spiralRadius: Float,
    spiralHeight: Float,
    numTurns: Float = 2.5f,
    ribbonWidth: Float,
    ribbonThickness: Float,
    ribbonHeight: Float,
    rotationDegrees: Float = 0.0f,
    value: Float = 1.0f,
  ): Unit =
    val numPoints = 300
    val angleStep = (numTurns * 2.0f * Math.PI.toFloat) / numPoints
    val rotationRad = rotationDegrees * Math.PI.toFloat / 180.0f
    val cosRot = Math.cos(rotationRad).toFloat
    val sinRot = Math.sin(rotationRad).toFloat

    for i <- 0 until numPoints do
      val t = i.toFloat / numPoints.toFloat

      val angle = -(i * angleStep)

      val spiralX = Math.cos(angle).toFloat * spiralRadius
      val spiralZ = Math.sin(angle).toFloat * spiralRadius
      val spiralY = spiralHeight * t

      val localX = spiralX * cosRot - spiralZ * sinRot
      val localZ = spiralX * sinRot + spiralZ * cosRot

      val x = centerX + localX
      val z = centerZ + localZ
      val y = centerY + spiralY

      val tangentAngle = angle + Math.PI.toFloat / 2.0f
      val localTangentX = Math.cos(tangentAngle).toFloat
      val localTangentZ = Math.sin(tangentAngle).toFloat

      val tangentX = localTangentX * cosRot - localTangentZ * sinRot
      val tangentZ = localTangentX * sinRot + localTangentZ * cosRot

      val radialX = spiralX / spiralRadius
      val radialZ = spiralZ / spiralRadius
      val worldRadialX = radialX * cosRot - radialZ * sinRot
      val worldRadialZ = radialX * sinRot + radialZ * cosRot

      val halfWidth = ribbonWidth / 2.0f
      val halfThickness = ribbonThickness / 2.0f
      val halfHeight = ribbonHeight / 2.0f

      val corners = Seq(
        (-halfWidth, -halfThickness, -halfHeight),
        (-halfWidth, -halfThickness, +halfHeight),
        (-halfWidth, +halfThickness, -halfHeight),
        (-halfWidth, +halfThickness, +halfHeight),
        (+halfWidth, -halfThickness, -halfHeight),
        (+halfWidth, -halfThickness, +halfHeight),
        (+halfWidth, +halfThickness, -halfHeight),
        (+halfWidth, +halfThickness, +halfHeight),
      )

      var minWorldX = Float.MaxValue
      var maxWorldX = Float.MinValue
      var minWorldY = Float.MaxValue
      var maxWorldY = Float.MinValue
      var minWorldZ = Float.MaxValue
      var maxWorldZ = Float.MinValue

      for (tangentOffset, radialOffset, verticalOffset) <- corners do
        val worldX = x + tangentX * tangentOffset + worldRadialX * radialOffset
        val worldY = y + verticalOffset
        val worldZ = z + tangentZ * tangentOffset + worldRadialZ * radialOffset

        minWorldX = minWorldX min worldX
        maxWorldX = maxWorldX max worldX
        minWorldY = minWorldY min worldY
        maxWorldY = maxWorldY max worldY
        minWorldZ = minWorldZ min worldZ
        maxWorldZ = maxWorldZ max worldZ

      val minXi = minWorldX.toInt max 0
      val maxXi = maxWorldX.toInt min (gridSize - 1)
      val minYi = minWorldY.toInt max 0
      val maxYi = maxWorldY.toInt min (gridSize - 1)
      val minZi = minWorldZ.toInt max 0
      val maxZi = maxWorldZ.toInt min (gridSize - 1)

      if minXi <= maxXi && minYi <= maxYi && minZi <= maxZi then
        addBox(obstacles, gridSize, minX = minXi, maxX = maxXi, minY = minYi, maxY = maxYi, minZ = minZi, maxZ = maxZi, value = value)

  /** Add a disc perpendicular to X axis (circular in YZ plane) - for scalar fields.
    *
    * Creates a thin disc that faces along the X direction.
    *
    * @param buffer
    *   Field buffer to modify (Float32 buffer)
    * @param gridSize
    *   Grid resolution
    * @param centerX
    *   Center X position
    * @param centerY
    *   Center Y position
    * @param centerZ
    *   Center Z position
    * @param radius
    *   Disc radius (in YZ plane)
    * @param thickness
    *   Disc thickness (in X direction)
    * @param value
    *   Field value to set
    */
  def addDiscPerpToX(
    buffer: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    radius: Float,
    thickness: Float,
    value: Float,
  ): Unit =
    buffer.rewind()
    val halfThickness = thickness / 2.0f

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val dx = x.toFloat - centerX
          if Math.abs(dx) <= halfThickness then
            val dy = y.toFloat - centerY
            val dz = z.toFloat - centerZ
            val distYZ = Math.sqrt(dy * dy + dz * dz).toFloat

            if distYZ <= radius then
              val idx = x + y * gridSize + z * gridSize * gridSize
              buffer.position(idx * 4)
              val existingValue = buffer.getFloat(idx * 4)
              buffer.position(idx * 4)
              buffer.putFloat(existingValue + value)

    buffer.rewind()

  /** Add velocity in a disc perpendicular to X axis (circular in YZ plane) - for Vec4 velocity fields.
    *
    * Creates a thin disc with velocity that faces along the X direction.
    *
    * @param velocityBuffer
    *   Velocity field buffer to modify (Vec4[Float32] = 16 bytes per cell)
    * @param gridSize
    *   Grid resolution
    * @param centerX
    *   Center X position
    * @param centerY
    *   Center Y position
    * @param centerZ
    *   Center Z position
    * @param radius
    *   Disc radius (in YZ plane)
    * @param thickness
    *   Disc thickness (in X direction)
    * @param velocityX
    *   Velocity X component
    * @param velocityY
    *   Velocity Y component
    * @param velocityZ
    *   Velocity Z component
    */
  def addDiscVecPerpToX(
    velocityBuffer: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    radius: Float,
    thickness: Float,
    velocityX: Float,
    velocityY: Float,
    velocityZ: Float,
  ): Unit =
    velocityBuffer.rewind()
    val halfThickness = thickness / 2.0f

    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val dx = x.toFloat - centerX
          if Math.abs(dx) <= halfThickness then
            val dy = y.toFloat - centerY
            val dz = z.toFloat - centerZ
            val distYZ = Math.sqrt(dy * dy + dz * dz).toFloat

            if distYZ <= radius then
              val idx = x + y * gridSize + z * gridSize * gridSize
              velocityBuffer.position(idx * 16)

              val existingX = velocityBuffer.getFloat()
              val existingY = velocityBuffer.getFloat()
              val existingZ = velocityBuffer.getFloat()

              velocityBuffer.position(idx * 16)
              velocityBuffer.putFloat(existingX + velocityX)
              velocityBuffer.putFloat(existingY + velocityY)
              velocityBuffer.putFloat(existingZ + velocityZ)
              velocityBuffer.putFloat(0.0f)

    velocityBuffer.rewind()
