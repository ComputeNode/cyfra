package io.computenode.cyfra.fluids.runner

import io.computenode.cyfra.core.{GBufferRegion, GCodec, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.*
import io.computenode.cyfra.fluids.visualization.RayMarchRenderer.Field.{Density, Dye, Pressure, Temperature, Velocity}
import io.computenode.cyfra.fluids.visualization.{Camera3D, RayMarchRenderer}
import io.computenode.cyfra.fluids.visualization.RayMarchRenderer.{RenderLayout, RenderParams, RendererConfig}
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import io.computenode.cyfra.utility.Logger.logger
import org.lwjgl.BufferUtils

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Paths
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import scala.util.chaining.*

/** Full fluid simulation with all solver steps chained in GExecution pipeline.
  * 
  * Pipeline:
  * 1. Forces (buoyancy)
  * 2. Advection (transport fields)
  * 3. Diffusion (viscosity)
  * 4. Divergence computation
  * 5. Pressure solve
  * 6. Projection (subtract pressure gradient)
  * 7. Boundary conditions
  */
object FullFluidSimulation:
  
  /** Complete execution layout with double-buffered state */
  case class SimulationLayout(
    velocityCurrent: GBuffer[Vec4[Float32]],
    velocityPrevious: GBuffer[Vec4[Float32]],
    densityCurrent: GBuffer[Float32],
    densityPrevious: GBuffer[Float32],
    temperatureCurrent: GBuffer[Float32],
    temperaturePrevious: GBuffer[Float32],
    dyeCurrent: GBuffer[Float32],
    dyePrevious: GBuffer[Float32],
    pressureCurrent: GBuffer[Float32],
    pressurePrevious: GBuffer[Float32],
    divergence: GBuffer[Float32],
    obstacles: GBuffer[Float32],
    fluidParams: GUniform[FluidParams],
    renderParams: GUniform[RenderParams],
    camera: GUniform[Camera3D],
    imageOutput: GBuffer[Vec4[Float32]],
  ) extends Layout:
    
    def toFluidStateCurrent: FluidState =
      FluidState(
        velocity = velocityCurrent,
        pressure = pressureCurrent,
        density = densityCurrent,
        temperature = temperatureCurrent,
        dye = dyeCurrent,
        divergence = divergence,
        obstacles = obstacles,
        params = fluidParams
      )
      
    def toFluidStatePrevious: FluidState =
      FluidState(
        velocity = velocityPrevious,
        pressure = pressurePrevious,
        density = densityPrevious,
        temperature = temperaturePrevious,
        dye = dyePrevious,
        divergence = divergence,
        obstacles = obstacles,
        params = fluidParams
      )

    def toFluidStateDouble: FluidStateDouble =
      FluidStateDouble(
        velocityCurrent = velocityCurrent,
        pressureCurrent = pressureCurrent,
        densityCurrent = densityCurrent,
        temperatureCurrent = temperatureCurrent,
        dyeCurrent = dyeCurrent,
        divergenceCurrent = divergence,
        velocityPrevious = velocityPrevious,
        pressurePrevious = pressurePrevious,
        densityPrevious = densityPrevious,
        temperaturePrevious = temperaturePrevious,
        dyePrevious = dyePrevious,
        divergencePrevious = divergence,
        obstacles = obstacles,
        params = fluidParams
      )

    def toFluidStateDoubleSwap: FluidStateDouble =
      FluidStateDouble(
        velocityCurrent = velocityPrevious,
        pressureCurrent = pressurePrevious,
        densityCurrent = densityPrevious,
        temperatureCurrent = temperaturePrevious,
        dyeCurrent = dyePrevious,
        divergenceCurrent = divergence,
        velocityPrevious = velocityCurrent,
        pressurePrevious = pressureCurrent,
        densityPrevious = densityCurrent,
        temperaturePrevious = temperatureCurrent,
        dyePrevious = dyeCurrent,
        divergencePrevious = divergence,
        obstacles = obstacles,
        params = fluidParams
      )

    def swap: SimulationLayout =
      this.copy(
        velocityCurrent = velocityPrevious,
        velocityPrevious = velocityCurrent,
        densityCurrent = densityPrevious,
        densityPrevious = densityCurrent,
        temperatureCurrent = temperaturePrevious,
        temperaturePrevious = temperatureCurrent,
        dyeCurrent = dyePrevious,
        dyePrevious = dyeCurrent,
      )

  /** Build complete simulation pipeline with all solver steps */
  def buildPipeline(renderDim: (Int, Int), jacobiIters: Int): GExecution[Int, SimulationLayout, SimulationLayout] =
    GExecution[Int, SimulationLayout]()
      // 1. Forces - Apply to velocityPrevious buffer (in-place)
      .addProgram(ForcesProgram.create)(
        totalCells => totalCells,
        _.toFluidStatePrevious
      )
      // 1b. Vorticity Confinement - Add swirling motion to velocityPrevious
      .addProgram(VorticityConfinementProgram.create)(
        totalCells => totalCells,
        _.toFluidStatePrevious
      )
      // 2. Advection - Reads velocityPrevious WITH forces, writes velocityCurrent
      .addProgram(AdvectionProgram.create)(
        totalCells => totalCells,
        _.toFluidStateDouble
      )
      // 3. Diffusion - Apply viscosity to velocity
      .addProgram(DiffusionProgram.create)(
        totalCells => totalCells,
        _.toFluidStateDouble
      )
      // 4. Projection - Enforce incompressibility (∇·u = 0)
      // 4a. Compute divergence
      .addProgram(ProjectionProgram.divergence)(
        totalCells => totalCells,
        _.toFluidStateCurrent
      )
      // 4b. Solve Poisson equation for pressure (40 Jacobi iterations)
      .addProgram(ProjectionProgram.pressureSolve)(
        totalCells => totalCells,
        _.toFluidStateDouble
      )
      // Solve with jacobi iteration
      .pipe: ex =>
        LazyList.iterate(ex): nex =>
          nex.addProgram(ProjectionProgram.pressureSolve)(totalCells => totalCells, _.toFluidStateDouble)
            .addProgram(ProjectionProgram.pressureSolve)(totalCells => totalCells, _.toFluidStateDoubleSwap)
        .apply(jacobiIters)
      // 4c. Subtract pressure gradient from velocity
      .addProgram(ProjectionProgram.subtractGradient)(
        totalCells => totalCells,
        _.toFluidStateCurrent
      )
      // 5. Boundary conditions - apply AFTER all simulation steps
      .addProgram(OutflowBoundaryProgram.create)(
        totalCells => totalCells,
        _.toFluidStateCurrent
      )
      // 6. Render
      .addProgram(RayMarchRenderer(RendererConfig(
        width = renderDim._1,
        height = renderDim._2,
        fieldToRender = Dye,
        renderOver = 0.0f,
        renderMax = 1.0f
      )).renderProgram)(
        totalCells => totalCells,
        l => RenderLayout(
          l.imageOutput,
          velocity = l.velocityCurrent,
          pressure = l.pressureCurrent,
          density = l.densityCurrent,
          temperature = l.temperatureCurrent,
          dye = l.dyeCurrent,
          l.obstacles,
          l.camera,
          l.renderParams
        )
      )
  

  @main def runFullFluidSimulation(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      logger.info("=== Full Fluid Simulation (All Solver Steps) ===")
      
      // Parameters
      val gridSize = 128
      val totalCells = gridSize * gridSize * gridSize
      val imageSize = (512, 512)
      val numFrames = 1000
      
      logger.info(s"Grid: ${gridSize}³ = $totalCells cells")
      logger.info(s"Image: ${imageSize._1}×${imageSize._2}")
      logger.info(s"Frames: $numFrames")
      
      // Build complete simulation pipeline      
      logger.info("Building complete simulation pipeline...")
      val simPipeline = buildPipeline(imageSize, 64)

      // Initialize parameters with stronger buoyancy
      val params = FluidParams(
        dt = 0.05f,              // Time step
        viscosity = 0.001f,    // Low viscosity (smoke is not very viscous)
        diffusion = 0.00001f,     // Slight diffusion
        buoyancy = 0.001f,        // Strong buoyancy (hot smoke rises!)
        ambient = 0.005f,         // Ambient temperature
        gridSize = gridSize,
        windX = 0f,           // Wind in X direction
        windY = 0.0f,
        windZ = 0f            // Wind in Z direction
      )
      val paramsBuffer = createParamsBuffer(params)
      
        // Camera setup
        // Grid is at Y=0 (bottom) to Y=gridSize (top), so look at middle height
        val cameraHeight = gridSize * 0f  // Camera slightly below center
        val lookAtCenter = (gridSize / 2.0f, gridSize / 2.0f, gridSize / 2.0f)  // Look at grid center
      
      // Initialize state buffers      
      logger.info("Initializing fluid state...")
      val velocityCurrent = createZeroVec4Buffer(totalCells)
      val velocityPrevious = createZeroVec4Buffer(totalCells)
      val densityCurrent = createZeroFloatBuffer(totalCells)
      val densityPrevious = createZeroFloatBuffer(totalCells)
      val temperatureCurrent = createZeroFloatBuffer(totalCells)
      val temperaturePrevious = createZeroFloatBuffer(totalCells)
      val dyeCurrent = createZeroFloatBuffer(totalCells)
      val dyePrevious = createZeroFloatBuffer(totalCells)
      val pressureCurrent = createZeroFloatBuffer(totalCells)
      val pressurePrevious = createZeroFloatBuffer(totalCells)
      val divergence = createZeroFloatBuffer(totalCells)
      val renderParamsStride = totalStride(summon[GStructSchema[RenderParams]])
      val renderParamsBuffer = BufferUtils.createByteBuffer(renderParamsStride)
      val cameraStride = totalStride(summon[GStructSchema[Camera3D]])
      val cameraBuffer = BufferUtils.createByteBuffer(cameraStride)

      logger.info("Creating obstacles...")
      val obstacles = FieldUtils.createEmpty(gridSize)

      // Set uniform density everywhere
      FieldUtils.addBox(
        densityCurrent,
        gridSize,
        minX = 0,
        maxX = gridSize - 1,
        minY = 0,
        maxY = gridSize - 1,
        minZ = 0,
        maxZ = gridSize - 1,
        value = 0.5f
      )

      // Uniform temperature (optional, for visualization)
      FieldUtils.addBox(
        temperatureCurrent,
        gridSize,
        minX = 0,
        maxX = gridSize - 1,
        minY = 0,
        maxY = gridSize - 1,
        minZ = 0,
        maxZ = gridSize - 1,
        value = 0.5f
      )

      // Add dye discs at the velocity current source locations
      // Discs are perpendicular to flow (X direction), circular in YZ plane
      val center = gridSize / 2.0f
      val currentRadius = gridSize * 0.15f
      val separation = gridSize * 0.35f
      val discThickness = gridSize * 0.05f  // Thin disc in X direction

      // Create the spiral obstacle (Scala logo with constant radius and ribbon shape!)
      addSpiralObstacle(
        obstacles,
        gridSize,
        centerX = center,
        centerY = gridSize * 0.4f,
        centerZ = center,
        spiralRadius = gridSize * 0.1f,
        spiralHeight = gridSize * 0.3f,
        numTurns = 2.5f,
        ribbonWidth = gridSize * 0.08f, // Width along curve (tangent)
        ribbonThickness = gridSize * 0.03f, // Thickness perpendicular to curve (thin!)
        ribbonHeight = gridSize * 0.07f, // Vertical height of ribbon
        rotationDegrees = 180f, // Can adjust to rotate the spiral
        value = 1.0f
      )

      val region = 
        (0 until numFrames).foldLeft(
          GBufferRegion
            .allocate[SimulationLayout]
        ): (regionAcc, frameIdx) =>
          regionAcc.map: layout =>

            logger.info(s"Frame $frameIdx / $numFrames")

            layout.velocityPrevious.read(velocityPrevious)
            layout.dyePrevious.read(dyePrevious)

            addCollidingDiscCurrents(velocityPrevious, dyePrevious, gridSize, frameIdx)
    
            // Prepare output buffer for reading back
            val outputBuffer = BufferUtils.createFloatBuffer(imageSize._1 * imageSize._2 * 4)
            val outputBB = org.lwjgl.system.MemoryUtil.memByteBuffer(outputBuffer)
            
            val angle = 90
            val angleRad = angle * Math.PI.toFloat / 180.0f
            val radius = gridSize * 1.4f
    
            // Prepare camera buffer
            val camera = Camera3D.orbit(
              centerX = lookAtCenter._1,
              centerY = lookAtCenter._2,
              centerZ = lookAtCenter._3,
              radius = radius,
              angle = angleRad,
              height = cameraHeight,
              aspectRatio = imageSize._1.toFloat / imageSize._2.toFloat
            )

            summon[GCodec[Camera3D, Camera3D]].toByteBuffer(cameraBuffer, Array(camera))
    
            // Prepare params buffer
            val renderParams = RenderParams(gridSize = gridSize)
            summon[GCodec[RenderParams, RenderParams]].toByteBuffer(renderParamsBuffer, Array(renderParams))

            // Write back prepared data to PREVIOUS buffers (simulation reads from Previous)
            layout.velocityPrevious.write(velocityPrevious)
            layout.dyePrevious.write(dyePrevious)
            // NOTE: write() with ByteBuffer does CPU->GPU transfer
            // write() with case class is a shader op (GIO) - wrong!
            layout.renderParams.write(renderParamsBuffer)
            layout.camera.write(cameraBuffer)

            // Run complete simulation pipeline
            logger.debug("Running simulation pipeline...")
            simPipeline.execute(totalCells, layout)
            
            // Read GPU-rendered image back to CPU
            layout.imageOutput.read(outputBB)
    
            velocityPrevious.rewind()
            dyePrevious.rewind()
    
            // Rewind the output buffer before reading
            outputBuffer.rewind()
            
            val outputPixels = ByteBuffer.allocateDirect(imageSize._1 * imageSize._2 * 4)
            while outputBuffer.hasRemaining do
              val pixel = (Math.clamp(outputBuffer.get(), 0.0f, 1.0f) * 255).toByte
              outputPixels.put(pixel)
    
            saveFrame(outputPixels, (imageSize._1, imageSize._2), s"smoke/full_fluid_$frameIdx.png")
    
            logger.debug(s"Saved full_fluid_$frameIdx.png")
    
            // CRITICAL: Swap buffers so Current becomes Previous for next frame
            // Without this, results don't propagate between frames!
            layout.swap 
          
      val resultLayout = region.runUnsafe(
            init = SimulationLayout(
              velocityCurrent = GBuffer(velocityCurrent),
              velocityPrevious = GBuffer(velocityPrevious),
              densityCurrent = GBuffer(densityCurrent),
              densityPrevious = GBuffer(densityPrevious),
              temperatureCurrent = GBuffer(temperatureCurrent),
              temperaturePrevious = GBuffer(temperaturePrevious),
              dyeCurrent = GBuffer(dyeCurrent),
              dyePrevious = GBuffer(dyePrevious),
              pressureCurrent = GBuffer(pressureCurrent),
              pressurePrevious = GBuffer(pressurePrevious),
              divergence = GBuffer(divergence),
              obstacles = GBuffer(obstacles),
              fluidParams = GUniform(paramsBuffer),
              renderParams = GUniform(renderParamsBuffer),
              camera = GUniform(cameraBuffer),
              imageOutput = GBuffer[Vec4[Float32]](imageSize._1 * imageSize._2)
            ),
            onDone = layout =>
              logger.info(s"Done! Created $numFrames frames")
              logger.info("Generate video with: ffmpeg -framerate 10 -i smoke/full_fluid_%d.png -c:v libx264 -pix_fmt yuv420p full_fluid.mp4")
          )


      
    finally
      runtime.close()

  /** Add a spiral obstacle pattern inspired by Scala logo.
    * 
    * Creates a 3D ribbon-shaped spiral with CONSTANT radius.
    * 
    * RIBBON SHAPE means:
    * - WIDE along the curve (tangent direction) 
    * - THIN perpendicular to the curve in horizontal plane (radial direction)
    * - Has VERTICAL HEIGHT
    * 
    * @param obstacles Obstacle buffer to modify
    * @param gridSize Grid resolution
    * @param centerX Center X position
    * @param centerY Center Y position
    * @param centerZ Center Z position
    * @param spiralRadius Constant radius of the spiral
    * @param spiralHeight Total height of the spiral
    * @param numTurns Number of complete rotations
    * @param ribbonWidth Width along the curve (tangent)
    * @param ribbonThickness Thickness perpendicular to curve (radial - thin!)
    * @param ribbonHeight Vertical height of the ribbon
    * @param rotationDegrees Rotation around Y axis in degrees
    * @param value Obstacle value (typically 1.0 for solid)
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
    value: Float = 1.0f
  ): Unit =
    // Generate many points along the spiral for smooth coverage
    val numPoints = 300
    val angleStep = (numTurns * 2.0f * Math.PI.toFloat) / numPoints
    val rotationRad = rotationDegrees * Math.PI.toFloat / 180.0f
    val cosRot = Math.cos(rotationRad).toFloat
    val sinRot = Math.sin(rotationRad).toFloat
    
    for i <- 0 until numPoints do
      val t = i.toFloat / numPoints.toFloat
      
      // CLOCKWISE when viewed from top (negate angle)
      val angle = -(i * angleStep)
      
      // Position on spiral (constant radius)
      val spiralX = Math.cos(angle).toFloat * spiralRadius
      val spiralZ = Math.sin(angle).toFloat * spiralRadius
      val spiralY = spiralHeight * t
      
      // Apply Y-axis rotation
      val localX = spiralX * cosRot - spiralZ * sinRot
      val localZ = spiralX * sinRot + spiralZ * cosRot
      
      // Translate to world position
      val x = centerX + localX
      val z = centerZ + localZ
      val y = centerY + spiralY
      
      // Tangent direction = direction of curve = perpendicular to radius
      // In local space before rotation
      val tangentAngle = angle + Math.PI.toFloat / 2.0f
      val localTangentX = Math.cos(tangentAngle).toFloat
      val localTangentZ = Math.sin(tangentAngle).toFloat
      
      // Apply rotation to tangent
      val tangentX = localTangentX * cosRot - localTangentZ * sinRot
      val tangentZ = localTangentX * sinRot + localTangentZ * cosRot
      
      // Radial direction = pointing from center to point on circle
      // Apply rotation to radial
      val radialX = spiralX / spiralRadius  // Normalized
      val radialZ = spiralZ / spiralRadius
      val worldRadialX = radialX * cosRot - radialZ * sinRot
      val worldRadialZ = radialX * sinRot + radialZ * cosRot
      
      // Now create a box segment:
      // - Extends ±ribbonWidth/2 along tangent
      // - Extends ±ribbonThickness/2 along radial (thin!)
      // - Extends ±ribbonHeight/2 vertically
      
      val halfWidth = ribbonWidth / 2.0f
      val halfThickness = ribbonThickness / 2.0f
      val halfHeight = ribbonHeight / 2.0f
      
      // Calculate 8 corners of the ribbon box
      // Corner offsets: (tangent, radial, vertical)
      val corners = Seq(
        (-halfWidth, -halfThickness, -halfHeight),
        (-halfWidth, -halfThickness, +halfHeight),
        (-halfWidth, +halfThickness, -halfHeight),
        (-halfWidth, +halfThickness, +halfHeight),
        (+halfWidth, -halfThickness, -halfHeight),
        (+halfWidth, -halfThickness, +halfHeight),
        (+halfWidth, +halfThickness, -halfHeight),
        (+halfWidth, +halfThickness, +halfHeight)
      )
      
      // Find bounding box in world space
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
      
      // Convert to grid indices
      val minX = minWorldX.toInt max 0
      val maxX = maxWorldX.toInt min (gridSize - 1)
      val minY = minWorldY.toInt max 0
      val maxY = maxWorldY.toInt min (gridSize - 1)
      val minZ = minWorldZ.toInt max 0
      val maxZ = maxWorldZ.toInt min (gridSize - 1)
      
      // Add the box if valid
      if minX <= maxX && minY <= maxY && minZ <= maxZ then
        FieldUtils.addBox(
          obstacles,
          gridSize,
          minX = minX,
          maxX = maxX,
          minY = minY,
          maxY = maxY,
          minZ = minZ,
          maxZ = maxZ,
          value = value
        )



  /** Add a disc perpendicular to X axis (circular in YZ plane) - for scalar fields.
    * 
    * Creates a thin disc that faces along the X direction.
    * 
    * @param buffer Field buffer to modify (Float32 buffer)
    * @param gridSize Grid resolution
    * @param centerX Center X position
    * @param centerY Center Y position
    * @param centerZ Center Z position
    * @param radius Disc radius (in YZ plane)
    * @param thickness Disc thickness (in X direction)
    * @param value Field value to set
    */
  def addDiscPerpToX(
    buffer: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    radius: Float,
    thickness: Float,
    value: Float
  ): Unit =
    buffer.rewind()
    val halfThickness = thickness / 2.0f
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          // Check if within thickness in X direction
          val dx = x.toFloat - centerX
          if Math.abs(dx) <= halfThickness then
            // Check if within radius in YZ plane
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
    * @param velocityBuffer Velocity field buffer to modify (Vec4[Float32] = 16 bytes per cell)
    * @param gridSize Grid resolution
    * @param centerX Center X position
    * @param centerY Center Y position
    * @param centerZ Center Z position
    * @param radius Disc radius (in YZ plane)
    * @param thickness Disc thickness (in X direction)
    * @param velocityX Velocity X component
    * @param velocityY Velocity Y component
    * @param velocityZ Velocity Z component
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
    velocityZ: Float
  ): Unit =
    velocityBuffer.rewind()
    val halfThickness = thickness / 2.0f
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          // Check if within thickness in X direction
          val dx = x.toFloat - centerX
          if Math.abs(dx) <= halfThickness then
            // Check if within radius in YZ plane
            val dy = y.toFloat - centerY
            val dz = z.toFloat - centerZ
            val distYZ = Math.sqrt(dy * dy + dz * dz).toFloat
            
            if distYZ <= radius then
              val idx = x + y * gridSize + z * gridSize * gridSize
              velocityBuffer.position(idx * 16) // Vec4 = 16 bytes
              
              // Read existing velocity
              val existingX = velocityBuffer.getFloat()
              val existingY = velocityBuffer.getFloat()
              val existingZ = velocityBuffer.getFloat()
              
              // Write new velocity (add to existing)
              velocityBuffer.position(idx * 16)
              velocityBuffer.putFloat(existingX + velocityX)
              velocityBuffer.putFloat(existingY + velocityY)
              velocityBuffer.putFloat(existingZ + velocityZ)
              velocityBuffer.putFloat(0.0f) // w component
    
    velocityBuffer.rewind()

  /** Add colliding disc-shaped velocity currents.
    * 
    * This should be called EVERY FRAME to continuously inject velocity.
    * Creates two disc-shaped regions with strong velocities flowing toward each other.
    * 
    * @param velocityBuffer Velocity field to modify
    * @param gridSize Grid resolution
    */
  def addCollidingDiscCurrents(
    velocityBuffer: ByteBuffer,
    dyeBuffer: ByteBuffer,
    gridSize: Int,
    frameIdx: Int
  ): Unit =
    val center = gridSize / 2.0f
    val currentRadius = gridSize * 0.15f
    val separation = gridSize * 0.35f
    val discThickness = gridSize * 0.05f
    val collisionSpeed = 2.0f
    
    val leftDiscX = center - separation
    val rightDiscX = center + separation
    
    // Left disc velocity - flowing RIGHT (positive X direction)
    addDiscVecPerpToX(
      velocityBuffer,
      gridSize,
      centerX = leftDiscX,
      centerY = center,
      centerZ = center,
      radius = currentRadius * 3f,
      thickness = discThickness * 100f,
      velocityX = collisionSpeed,
      velocityY = 0.0f,
      velocityZ = 0.0f
    )

    if frameIdx < 20 then
      addDiscPerpToX(
        dyeBuffer,
        gridSize,
        centerX = leftDiscX,
        centerY = center,
        centerZ = center,
        radius = currentRadius,
        thickness = discThickness,
        value = 0.5f
      )



  /** Copy buffer contents */
  def copyBuffer(src: ByteBuffer, dst: ByteBuffer): Unit =
    src.rewind()
    dst.rewind()
    dst.put(src)
    src.rewind()
    dst.rewind()

  /** Create FluidParams buffer */
  def createParamsBuffer(params: FluidParams): ByteBuffer =
    import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
    import io.computenode.cyfra.dsl.struct.GStruct.given
    import io.computenode.cyfra.core.GCodec
    
    val schema = summon[io.computenode.cyfra.dsl.struct.GStructSchema[FluidParams]]
    val size = totalStride(schema)
    val buffer = ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder())
    
    summon[GCodec[FluidParams, FluidParams]].toByteBuffer(buffer, Array(params))
    buffer.rewind()
    buffer

  /** Create zero Vec4 buffer */
  def createZeroVec4Buffer(totalCells: Int): ByteBuffer =
    ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())

  /** Create zero Float32 buffer */
  def createZeroFloatBuffer(totalCells: Int): ByteBuffer =
    ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())

  /** Save frame as PNG (flip Y: image Y=0 is top, but 3D Y=0 is bottom) */
  def saveFrame(pixels: ByteBuffer, imageSize: (Int, Int), filename: String): Unit =
    val (width, height) = imageSize
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    
    pixels.rewind()
    for y <- 0 until height; x <- 0 until width do
      val r = (pixels.get() & 0xFF)
      val g = (pixels.get() & 0xFF)
      val b = (pixels.get() & 0xFF)
      val a = (pixels.get() & 0xFF)
      
      val rgb = (r << 16) | (g << 8) | b
      // Flip Y coordinate: image Y=0 is top, but 3D Y=0 should be at bottom of image
      image.setRGB(x, height - 1 - y, rgb)
    
    val file = Paths.get(filename).toFile
    Option(file.getParentFile).foreach(_.mkdirs())
    ImageIO.write(image, "png", file)

