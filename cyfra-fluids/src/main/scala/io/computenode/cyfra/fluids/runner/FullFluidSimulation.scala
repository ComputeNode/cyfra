package io.computenode.cyfra.fluids.runner

import io.computenode.cyfra.core.{GBufferRegion, GCodec, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.fluids.solver.*
import io.computenode.cyfra.fluids.visualization.RayMarchRenderer.Field.{Density, Pressure, Temperature, Velocity}
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
        divergenceCurrent = divergence,
        velocityPrevious = velocityPrevious,
        pressurePrevious = pressurePrevious,
        densityPrevious = densityPrevious,
        temperaturePrevious = temperaturePrevious,
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
        divergenceCurrent = divergence,
        velocityPrevious = velocityCurrent,
        pressurePrevious = pressureCurrent,
        densityPrevious = densityCurrent,
        temperaturePrevious = temperatureCurrent,
        divergencePrevious = divergence,
        obstacles = obstacles,
        params = fluidParams
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
      .addProgram(OutflowBoundaryProgram.create)(
        totalCells => totalCells,
        _.toFluidStateCurrent
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
      // 5. Boundary conditions
//      .addProgram(DissipativeBoundaryProgram.create)(
//        totalCells => totalCells,
//        _.toFluidStateCurrent
//      )
      // 6. Render
      .addProgram(RayMarchRenderer(RendererConfig(
        width = renderDim._1,
        height = renderDim._2,
        fieldToRender = Density,
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
      val numFrames = 2000
      
      logger.info(s"Grid: ${gridSize}³ = $totalCells cells")
      logger.info(s"Image: ${imageSize._1}×${imageSize._2}")
      logger.info(s"Frames: $numFrames")
      
      // Build complete simulation pipeline      
      logger.info("Building complete simulation pipeline...")
      val simPipeline = buildPipeline(imageSize, 128)

      // Initialize parameters with stronger buoyancy
      val params = FluidParams(
        dt = 0.1f,              // Time step
        viscosity = 0.001f,    // Low viscosity (smoke is not very viscous)
        diffusion = 0.001f,     // Slight diffusion
        buoyancy = 0.5f,        // Strong buoyancy (hot smoke rises!)
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
      
      logger.info("Allocating GPU memory...")
      val region = GBufferRegion
        .allocate[SimulationLayout]
        .map: region =>
          simPipeline.execute(totalCells, region)
      
      // Initialize state buffers      
      logger.info("Initializing fluid state...")
      val velocityCurrent = createZeroVec4Buffer(totalCells)
      val velocityPrevious = createZeroVec4Buffer(totalCells)
      val densityCurrent = createZeroFloatBuffer(totalCells)
      val densityPrevious = createZeroFloatBuffer(totalCells)
      val temperatureCurrent = createZeroFloatBuffer(totalCells)
      val temperaturePrevious = createZeroFloatBuffer(totalCells)
      val pressureCurrent = createZeroFloatBuffer(totalCells)
      val pressurePrevious = createZeroFloatBuffer(totalCells)
      val divergence = createZeroFloatBuffer(totalCells)
      
      logger.info("Creating obstacles...")
      val obstacles = FieldUtils.createEmpty(gridSize)
//      FieldUtils.addBox(
//        obstacles,
//        gridSize,
//        minX = (gridSize * 0.4f).toInt,
//        maxX = (gridSize * 0.6f).toInt,
//        minY = (gridSize * 0.2f).toInt,
//        maxY = (gridSize * 0.4f).toInt,
//        minZ = (gridSize * 0.4f).toInt,
//        maxZ = (gridSize * 0.6f).toInt,
//        0.8f
//      )

      // Option 1: Simple uniform initialization
//      FieldUtils.addBox(densityCurrent, gridSize, 0, gridSize, 0, gridSize, 0, gridSize, 0.4f)
//      FieldUtils.addBox(temperatureCurrent, gridSize, 0, gridSize, 0, gridSize, 0, gridSize, 0.4f)
      
      // Option 2: Setup with swirling vortices
      // setupSwirlFields(velocityCurrent, densityCurrent, temperatureCurrent, gridSize)
      
      // Option 3: Complex collision scene with multiple pockets
      // setupCollisionScene(velocityCurrent, densityCurrent, temperatureCurrent, gridSize)
      
      // Option 4: Wind tunnel with Scala spiral obstacle
      setupWindTunnelScene(velocityCurrent, densityCurrent, temperatureCurrent, obstacles, gridSize)

      // Main simulation loop
      for frameIdx <- 0 until numFrames do

//        FieldUtils.addBox(
//          densityCurrent,
//          gridSize,
//          minX = (gridSize * 0.4f).toInt,
//          maxX = (gridSize * 0.6f).toInt,
//          minY = (gridSize * 0.0f).toInt,
//          maxY = (gridSize * 0.1f).toInt,
//          minZ = (gridSize * 0.4f).toInt,
//          maxZ = (gridSize * 0.6f).toInt,
//          0.5f
//        )
//
//        FieldUtils.addBox(
//          temperatureCurrent,
//          gridSize,
//          minX = (gridSize * 0.4f).toInt,
//          maxX = (gridSize * 0.6f).toInt,
//          minY = (gridSize * 0.0f).toInt,
//          maxY = (gridSize * 0.1f).toInt,
//          minZ = (gridSize * 0.4f).toInt,
//          maxZ = (gridSize * 0.6f).toInt,
//          1f
//        )
        
        // Optional: Add swirling velocity at smoke source for continuous vortex generation
        // Uncomment to create ongoing swirling motion at the injection point:
//        addVortexVelocity(
//           velocityCurrent,
//           gridSize,
//           centerX = gridSize / 2.0f,
//           centerY = gridSize * 0.05f,
//           centerZ = gridSize / 2.0f,
//           strength = 3.0f,
//           radius = gridSize * 0.2f,
//           upwardForce = 1.0f
//        )
        
        // Optional: Wind tunnel continuous injection (for Option 4)
        // Maintains smoke flow from the inlet (back Z=0 plane)

        val t = frameIdx.toFloat / numFrames.toFloat
        logger.info(s"Frame $frameIdx / $numFrames")

        // Add smoke source at bottom center
        logger.debug("Adding smoke source...")

        
        // Copy current to previous for this timestep
        copyBuffer(velocityCurrent, velocityPrevious)
        copyBuffer(densityCurrent, densityPrevious)
        copyBuffer(temperatureCurrent, temperaturePrevious)
        copyBuffer(pressureCurrent, pressurePrevious)
        
        // Rendering buffers 

        // Prepare output buffer for reading back
        val outputData = Array.ofDim[Float](imageSize._1 * imageSize._2 * 4)
        val outputBuffer = BufferUtils.createFloatBuffer(imageSize._1 * imageSize._2 * 4)
        val outputBB = org.lwjgl.system.MemoryUtil.memByteBuffer(outputBuffer)


        val angle = 0
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
        val cameraStride = totalStride(summon[GStructSchema[Camera3D]])
        val cameraBuffer = BufferUtils.createByteBuffer(cameraStride)
        summon[GCodec[Camera3D, Camera3D]].toByteBuffer(cameraBuffer, Array(camera))

        // Prepare params buffer
        val renderParamsStride = totalStride(summon[GStructSchema[RenderParams]])
        val renderParamsBuffer = BufferUtils.createByteBuffer(renderParamsStride)
        val renderParams = RenderParams(gridSize = gridSize)
        summon[GCodec[RenderParams, RenderParams]].toByteBuffer(renderParamsBuffer, Array(renderParams))
        
        // Run complete simulation pipeline
        logger.debug("Running simulation pipeline...")
        val resultLayout = region.runUnsafe(
          init = SimulationLayout(
            velocityCurrent = GBuffer(velocityCurrent),
            velocityPrevious = GBuffer(velocityPrevious),
            densityCurrent = GBuffer(densityCurrent),
            densityPrevious = GBuffer(densityPrevious),
            temperatureCurrent = GBuffer(temperatureCurrent),
            temperaturePrevious = GBuffer(temperaturePrevious),
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
            // Read back updated state
            layout.velocityCurrent.read(velocityCurrent.rewind())
            layout.densityCurrent.read(densityCurrent.rewind())
            layout.temperatureCurrent.read(temperatureCurrent.rewind())
            layout.pressureCurrent.read(pressureCurrent.rewind())
            layout.imageOutput.read(outputBB)
        )
        
        velocityCurrent.rewind()
        densityCurrent.rewind()
        temperatureCurrent.rewind()
        pressureCurrent.rewind()

        val outputPixels = ByteBuffer.allocateDirect(imageSize._1 * imageSize._2 * 4)
        while outputBuffer.hasRemaining do
          val pixel = (Math.clamp(outputBuffer.get(), 0.0f, 1.0f) * 255).toByte
          outputPixels.put(pixel)
        
        saveFrame(outputPixels, (imageSize._1, imageSize._2), s"smoke/full_fluid_$frameIdx.png")
        
        logger.debug(s"Saved full_fluid_$frameIdx.png")
      
      logger.info(s"Done! Created $numFrames frames")
      logger.info("Generate video with: ffmpeg -framerate 10 -i smoke/full_fluid_%d.png -c:v libx264 -pix_fmt yuv420p full_fluid.mp4")
      
    finally
      runtime.close()

  /** Simple noise function for adding irregularity */
  def simpleNoise(x: Float, y: Float, z: Float, time: Float): Float =
    val s = Math.sin(x * 0.5 + time * 0.3).toFloat
    val c = Math.cos(z * 0.5 + time * 0.2).toFloat
    val s2 = Math.sin((x + z) * 0.3 + time * 0.5).toFloat
    (s * c + s2 * 0.5f) * 0.5f + 0.5f

  /** Add rotational velocity field to create a vortex.
    * 
    * Creates circular motion around a vertical axis.
    * 
    * @param velocity Vec4 velocity buffer to modify
    * @param gridSize Grid resolution
    * @param centerX Vortex center X coordinate
    * @param centerY Vortex center Y coordinate (height)
    * @param centerZ Vortex center Z coordinate
    * @param strength Maximum tangential velocity
    * @param radius Radius of influence
    * @param upwardForce Vertical velocity component (positive = upward)
    */
  def addVortexVelocity(
    velocity: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    strength: Float = 5.0f,
    radius: Float = 10.0f,
    upwardForce: Float = 2.0f
  ): Unit =
    velocity.rewind()
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          velocity.position(idx * 16) // Vec4 = 16 bytes
          
          // Distance from vortex center in XZ plane
          val dx = x.toFloat - centerX
          val dz = z.toFloat - centerZ
          val dy = y.toFloat - centerY
          val distXZ = Math.sqrt(dx*dx + dz*dz).toFloat
          
          // Falloff based on distance (Gaussian-like)
          val distFactor = Math.exp(-distXZ * distXZ / (radius * radius)).toFloat
          val heightFactor = Math.exp(-dy * dy / (radius * radius * 0.5)).toFloat
          val combinedFactor = distFactor * heightFactor
          
          // Create circular motion (tangent to radius vector)
          // Velocity perpendicular to radius vector in XZ plane
          val velX = if (distXZ > 0.01f) (-dz / distXZ) * strength * combinedFactor else 0.0f
          val velZ = if (distXZ > 0.01f) (dx / distXZ) * strength * combinedFactor else 0.0f
          val velY = upwardForce * combinedFactor
          
          // Read existing velocity and add vortex component
          val existingX = velocity.getFloat(idx * 16)
          val existingY = velocity.getFloat(idx * 16 + 4)
          val existingZ = velocity.getFloat(idx * 16 + 8)
          
          velocity.position(idx * 16)
          velocity.putFloat(existingX + velX)
          velocity.putFloat(existingY + velY)
          velocity.putFloat(existingZ + velZ)
          velocity.putFloat(0.0f) // w component
    
    velocity.rewind()

  /** Add counter-rotating vortex pair for more complex swirling patterns.
    * 
    * Creates two vortices rotating in opposite directions, which creates
    * interesting turbulent interactions.
    * 
    * @param velocity Vec4 velocity buffer to modify
    * @param gridSize Grid resolution
    * @param centerX Center X coordinate between the two vortices
    * @param centerY Center Y coordinate (height)
    * @param centerZ Center Z coordinate
    * @param separation Distance between vortex centers
    * @param strength Maximum tangential velocity
    * @param radius Radius of influence for each vortex
    */
  def addCounterRotatingVortices(
    velocity: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    separation: Float = 10.0f,
    strength: Float = 5.0f,
    radius: Float = 8.0f
  ): Unit =
    // First vortex (clockwise when viewed from above)
    addVortexVelocity(
      velocity,
      gridSize,
      centerX - separation / 2.0f,
      centerY,
      centerZ,
      strength,
      radius,
      upwardForce = 2.0f
    )
    
    // Second vortex (counter-clockwise when viewed from above)
    addVortexVelocity(
      velocity,
      gridSize,
      centerX + separation / 2.0f,
      centerY,
      centerZ,
      -strength,  // Negative strength for opposite rotation
      radius,
      upwardForce = 2.0f
    )

  /** Setup initial swirling fields for smoke simulation.
    * 
    * Creates an initial velocity configuration with multiple vortices
    * and sets up density/temperature fields to visualize the swirling motion.
    * 
    * @param velocityBuffer Velocity field to initialize
    * @param densityBuffer Density field to initialize  
    * @param temperatureBuffer Temperature field to initialize
    * @param gridSize Grid resolution
    */
  def setupSwirlFields(
    velocityBuffer: ByteBuffer,
    densityBuffer: ByteBuffer,
    temperatureBuffer: ByteBuffer,
    gridSize: Int
  ): Unit =
    val center = gridSize / 2.0f
    
    // Add a strong central vortex at bottom-center
    addVortexVelocity(
      velocityBuffer,
      gridSize,
      centerX = center,
      centerY = gridSize * 0.15f,
      centerZ = center,
      strength = 6.0f,
      radius = gridSize * 0.3f,
      upwardForce = 0.3f
    )
    
    // Add counter-rotating vortices slightly higher up for complexity
    addCounterRotatingVortices(
      velocityBuffer,
      gridSize,
      centerX = center,
      centerY = gridSize * 0.3f,
      centerZ = center,
      separation = gridSize * 0.3f,
      strength = 4.0f,
      radius = gridSize * 0.2f
    )
    
    // Add initial smoke density in a column at the center
    FieldUtils.addCylinder(
      densityBuffer,
      gridSize,
      centerX = center,
      centerZ = center,
      radius = gridSize * 0.15f,
      minY = 0,
      maxY = (gridSize * 0.25f).toInt,
      value = 1f
    )
    
    // Add temperature to drive buoyancy
    FieldUtils.addCylinder(
      temperatureBuffer,
      gridSize,
      centerX = center,
      centerZ = center,
      radius = gridSize * 0.15f,
      minY = 0,
      maxY = (gridSize * 0.25f).toInt,
      value = 1.0f
    )

  /** Add directional velocity to a spherical region.
    * 
    * @param velocity Velocity buffer to modify
    * @param gridSize Grid resolution
    * @param centerX Center X position
    * @param centerY Center Y position
    * @param centerZ Center Z position
    * @param radius Sphere radius
    * @param velocityX Velocity in X direction
    * @param velocityY Velocity in Y direction
    * @param velocityZ Velocity in Z direction
    */
  def addDirectionalVelocity(
    velocity: ByteBuffer,
    gridSize: Int,
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    radius: Float,
    velocityX: Float,
    velocityY: Float,
    velocityZ: Float
  ): Unit =
    velocity.rewind()
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          
          // Calculate distance from center
          val dx = x.toFloat - centerX
          val dy = y.toFloat - centerY
          val dz = z.toFloat - centerZ
          val dist = Math.sqrt(dx*dx + dy*dy + dz*dz).toFloat
          
          // Apply velocity with smooth falloff
          if dist < radius then
            val falloff = Math.cos((dist / radius) * Math.PI * 0.5).toFloat
            falloff * falloff // Squared for smoother falloff
            
            velocity.position(idx * 16)
            val existingX = velocity.getFloat()
            val existingY = velocity.getFloat()
            val existingZ = velocity.getFloat()
            
            velocity.position(idx * 16)
            velocity.putFloat(existingX + velocityX * falloff)
            velocity.putFloat(existingY + velocityY * falloff)
            velocity.putFloat(existingZ + velocityZ * falloff)
            velocity.putFloat(0.0f)
    
    velocity.rewind()

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

  /** Setup wind tunnel scene with spiral obstacle.
    * 
    * Creates a wind tunnel simulation with wind flowing in +Z direction
    * (toward the camera when viewed from +X side), with a Scala logo spiral
    * obstacle in the center that creates turbulent flow patterns.
    * 
    * @param velocityBuffer Velocity field to initialize
    * @param densityBuffer Density field to initialize
    * @param temperatureBuffer Temperature field to initialize
    * @param obstacleBuffer Obstacle field to initialize
    * @param gridSize Grid resolution
    */
  def setupWindTunnelScene(
    velocityBuffer: ByteBuffer,
    densityBuffer: ByteBuffer,
    temperatureBuffer: ByteBuffer,
    obstacleBuffer: ByteBuffer,
    gridSize: Int
  ): Unit =
    val center = gridSize / 2.0f
    
    // Create the spiral obstacle (Scala logo with constant radius and ribbon shape!)
    addSpiralObstacle(
      obstacleBuffer,
      gridSize,
      centerX = center,
      centerY = gridSize * 0.2f,
      centerZ = center,
      spiralRadius = gridSize * 0.2f,
      spiralHeight = gridSize * 0.6f,
      numTurns = 2.5f,
      ribbonWidth = gridSize * 0.15f,        // Width along curve (tangent)
      ribbonThickness = gridSize * 0.03f,    // Thickness perpendicular to curve (thin!)
      ribbonHeight = gridSize * 0.1f,       // Vertical height of ribbon
      rotationDegrees = 60.0f,                // Can adjust to rotate the spiral
      value = 1.0f
    )

    // Initialize uniform wind flow in +Z direction (flows across your view from the side)
    val windSpeed = 2.0f
    velocityBuffer.rewind()
    
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val idx = x + y * gridSize + z * gridSize * gridSize
          velocityBuffer.position(idx * 16)
          
          // Uniform wind in +Z direction (perpendicular to side view)
          velocityBuffer.putFloat(0.0f)
          velocityBuffer.putFloat(0.0f)
          velocityBuffer.putFloat(windSpeed)
          velocityBuffer.putFloat(0.0f)
    
    velocityBuffer.rewind()
    
    // Add smoke/density at the inlet (back of the tunnel, Z=0)
    FieldUtils.addBox(
      densityBuffer,
      gridSize,
      minX = (gridSize * 0.4f).toInt,
      maxX = (gridSize * 0.6f).toInt,
      minY = (gridSize * 0.4f).toInt,
      maxY = (gridSize * 0.6f).toInt,
      minZ = 0,
      maxZ = (gridSize * 0.1f).toInt,
      value = 0.8f
    )
    
    // Add slight temperature variation for buoyancy effects
    FieldUtils.addBox(
      temperatureBuffer,
      gridSize,
      minX = (gridSize * 0.2f).toInt,
      maxX = (gridSize * 0.8f).toInt,
      minY = (gridSize * 0.2f).toInt,
      maxY = (gridSize * 0.7f).toInt,
      minZ = 0,
      maxZ = (gridSize * 0.15f).toInt,
      value = 0.6f
    )
    
    // Add some vortex generators at the inlet for turbulence
    for i <- 0 until 3 do
      val y = gridSize * (0.3f + i * 0.15f)
      val x = gridSize * (0.25f + i * 0.25f)
      
      addVortexVelocity(
        velocityBuffer,
        gridSize,
        centerX = x,
        centerY = y,
        centerZ = gridSize * 0.1f,
        strength = 2.0f,
        radius = gridSize * 0.08f,
        upwardForce = 0.0f
      )

  /** Setup complex collision scene with multiple hot pockets moving toward each other.
    * 
    * Creates multiple sources of density/temperature at different locations,
    * each with initial velocities that cause them to collide and interact.
    * Includes swirling motions for dramatic turbulent effects.
    * 
    * @param velocityBuffer Velocity field to initialize
    * @param densityBuffer Density field to initialize
    * @param temperatureBuffer Temperature field to initialize
    * @param gridSize Grid resolution
    */
  def setupCollisionScene(
    velocityBuffer: ByteBuffer,
    densityBuffer: ByteBuffer,
    temperatureBuffer: ByteBuffer,
    gridSize: Int
  ): Unit =
    val center = gridSize / 2.0f
    val third = gridSize / 3.0f
    
    // Define 6 "pockets" arranged in a ring, all moving toward center
    val numPockets = 6
    val pocketRadius = gridSize * 0.35f // Distance from center
    val pocketSize = gridSize * 0.12f
    val inwardSpeed = 4.0f
    val swirlingSpeed = 3.0f
    
    for i <- 0 until numPockets do
      val angle = (i.toFloat / numPockets.toFloat) * 2.0f * Math.PI.toFloat
      val x = center + Math.cos(angle).toFloat * pocketRadius
      val z = center + Math.sin(angle).toFloat * pocketRadius
      val y = gridSize * (0.15f + 0.05f * Math.sin(i * 1.5).toFloat) // Varying heights
      
      // Add density pocket (sphere)
      FieldUtils.addSphere(
        densityBuffer,
        gridSize,
        centerX = x,
        centerY = y,
        centerZ = z,
        radius = pocketSize,
        value = 0.8f + 0.2f * (i % 3) / 3.0f // Varying densities
      )
      
      // Add temperature pocket (hotter = more buoyancy)
      FieldUtils.addSphere(
        temperatureBuffer,
        gridSize,
        centerX = x,
        centerY = y,
        centerZ = z,
        radius = pocketSize * 0.9f,
        value = 0.9f + 0.1f * ((i + 1) % 2) // Alternating temperatures
      )
      
      // Calculate velocity toward center with swirling component
      val towardCenterX = (center - x) / pocketRadius * inwardSpeed
      val towardCenterZ = (center - z) / pocketRadius * inwardSpeed
      
      // Add tangential component for swirling
      val tangentX = -Math.sin(angle).toFloat * swirlingSpeed * (if i % 2 == 0 then 1 else -1)
      val tangentZ = Math.cos(angle).toFloat * swirlingSpeed * (if i % 2 == 0 then 1 else -1)
      
      // Add upward component
      val upwardSpeed = 1.5f + 0.5f * (i % 3)
      
      // Apply directional velocity to the pocket
      addDirectionalVelocity(
        velocityBuffer,
        gridSize,
        centerX = x,
        centerY = y,
        centerZ = z,
        radius = pocketSize * 1.2f,
        velocityX = towardCenterX + tangentX,
        velocityY = upwardSpeed,
        velocityZ = towardCenterZ + tangentZ
      )
    
    // Add a central upward vortex to enhance the collision dynamics
    addVortexVelocity(
      velocityBuffer,
      gridSize,
      centerX = center,
      centerY = gridSize * 0.2f,
      centerZ = center,
      strength = 5.0f,
      radius = gridSize * 0.25f,
      upwardForce = 2.0f
    )
    
    // Add three counter-rotating pairs at different heights for chaos
    for i <- 0 until 3 do
      val angle = (i.toFloat / 3.0f) * 2.0f * Math.PI.toFloat
      val offsetX = Math.cos(angle).toFloat * gridSize * 0.2f
      val offsetZ = Math.sin(angle).toFloat * gridSize * 0.2f
      
      addCounterRotatingVortices(
        velocityBuffer,
        gridSize,
        centerX = center + offsetX,
        centerY = gridSize * (0.3f + i * 0.15f),
        centerZ = center + offsetZ,
        separation = gridSize * 0.15f,
        strength = 3.0f * (1.0f - i * 0.2f),
        radius = gridSize * 0.12f
      )
    
    // Add some extra density pockets at top corners moving down
    for i <- 0 until 4 do
      val angle = i * Math.PI.toFloat / 2.0f + Math.PI.toFloat / 4.0f
      val x = center + Math.cos(angle).toFloat * gridSize * 0.4f
      val z = center + Math.sin(angle).toFloat * gridSize * 0.4f
      val y = gridSize * 0.7f
      
      FieldUtils.addSphere(
        densityBuffer,
        gridSize,
        centerX = x,
        centerY = y,
        centerZ = z,
        radius = gridSize * 0.08f,
        value = 0.6f
      )
      
      FieldUtils.addSphere(
        temperatureBuffer,
        gridSize,
        centerX = x,
        centerY = y,
        centerZ = z,
        radius = gridSize * 0.08f,
        value = 0.7f
      )
      
      // Downward spiral motion
      addDirectionalVelocity(
        velocityBuffer,
        gridSize,
        centerX = x,
        centerY = y,
        centerZ = z,
        radius = gridSize * 0.1f,
        velocityX = -Math.cos(angle).toFloat * 2.0f,
        velocityY = -3.0f,
        velocityZ = -Math.sin(angle).toFloat * 2.0f
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
    
    ImageIO.write(image, "png", Paths.get(filename).toFile)

