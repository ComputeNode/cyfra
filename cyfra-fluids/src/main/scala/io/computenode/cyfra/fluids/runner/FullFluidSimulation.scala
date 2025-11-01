package io.computenode.cyfra.fluids.runner

import io.computenode.cyfra.core.{GBufferRegion, GCodec, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.fluids.solver.*
import io.computenode.cyfra.fluids.visualization.{Camera3D, RayMarchRenderer}
import io.computenode.cyfra.fluids.visualization.RayMarchRenderer.{RenderLayout, RenderParams}
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
      // 1.5. Clear obstacles BEFORE advection (critical!)
      .addProgram(OutflowBoundaryProgram.create)(
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
      // 5. Boundary conditions (use OutflowBoundaryProgram for realistic smoke escape)
      .addProgram(OutflowBoundaryProgram.create)(
        totalCells => totalCells,
        _.toFluidStateCurrent
      )
      // 6. Render
      .addProgram(RayMarchRenderer(renderDim._1, renderDim._2).renderProgram)(
        totalCells => totalCells,
        l => RenderLayout(
          l.imageOutput,
          l.densityCurrent,
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
      val gridSize = 64
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
        dt = 0.5f,              // Time step
        viscosity = 0.00001f,    // Low viscosity (smoke is not very viscous)
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
        val cameraHeight = gridSize * 0.3f  // Camera slightly below center
        val lookAtCenter = (gridSize / 2.0f, gridSize / 2.0f, gridSize / 2.0f)  // Look at grid center
      
      // Allocate GPU memory region
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
      val obstacles = ObstacleUtils.createEmpty(gridSize)
      ObstacleUtils.addBox(
        obstacles,
        gridSize,
        minX = (gridSize * 0.4f).toInt,
        maxX = (gridSize * 0.6f).toInt,
        minY = (gridSize * 0.2f).toInt,
        maxY = (gridSize * 0.4f).toInt,
        minZ = (gridSize * 0.4f).toInt,
        maxZ = (gridSize * 0.6f).toInt,
        0.8f
      )

      // Main simulation loop
      for frameIdx <- 0 until numFrames do
        addSmokeSource(densityCurrent, temperatureCurrent, gridSize, 0)
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
        val radius = gridSize * 2f

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
  
  /** Add smoke source at bottom center with noise for irregularity */
  def addSmokeSource(
    densityBuffer: ByteBuffer,
    temperatureBuffer: ByteBuffer,
    gridSize: Int,
    frameIdx: Int
  ): Unit =
    val time = frameIdx
    val pulseFactor = 1.0f
    val sourceRadius = gridSize / 12f
    val centerX = gridSize / 2
    val centerZ = gridSize / 2
    
    // Add density at bottom with noise
    densityBuffer.rewind()
    for z <- 0 until gridSize; y <- 0 until gridSize; x <- 0 until gridSize do
      val idx = x + y * gridSize + z * gridSize * gridSize
      densityBuffer.position(idx * 4)
      
      if y < 6 then
        val dx = x - centerX
        val dz = z - centerZ
        val distSq = dx * dx + dz * dz
        val dist = Math.sqrt(distSq.toDouble).toFloat
        
        if distSq < sourceRadius * sourceRadius then
          val newVal = 0.8f
          densityBuffer.putFloat(newVal)
    
    // Add temperature with noise
    temperatureBuffer.rewind()
    for z <- 0 until gridSize; y <- 0 until gridSize; x <- 0 until gridSize do
      val idx = x + y * gridSize + z * gridSize * gridSize
      temperatureBuffer.position(idx * 4)
      
      if y < 2 then
        val dx = x - centerX
        val dz = z - centerZ
        val distSq = dx * dx + dz * dz
        val dist = Math.sqrt(distSq.toDouble).toFloat
        
        if distSq < sourceRadius * sourceRadius then
          // Add noise for irregular heat distribution
          val noise = simpleNoise(x.toFloat + 10, y.toFloat, z.toFloat + 10, time.toFloat)
          val falloff = 1.0f - (dist / sourceRadius)
          val strength = falloff * (0.4f + noise * 0.6f) * pulseFactor
          
          val existing = temperatureBuffer.getFloat()
          temperatureBuffer.position(idx * 4)
          val newVal = (existing + 1.5f * strength).min(3.0f) * 0.6f
          temperatureBuffer.putFloat(newVal)
    
    densityBuffer.rewind()
    temperatureBuffer.rewind()

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

