package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState, FluidStateDouble}
import io.computenode.cyfra.fluids.solver.*
import io.computenode.cyfra.fluids.visualization.{Camera3D, RayMarchRenderer}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Paths
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

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
    params: GUniform[FluidParams]
  ) extends Layout
  
  /** Build simplified pipeline (just forces for now - advection has SPIR-V bug) */
  def buildPipeline(): GExecution[Int, SimulationLayout, SimulationLayout] =
    GExecution[Int, SimulationLayout]()
      // 1. Forces (buoyancy) - VERIFIED WORKING
      .addProgram(ForcesProgram.create)(
        totalCells => totalCells,
        layout => FluidState(
          velocity = layout.velocityCurrent,
          pressure = layout.pressureCurrent,
          density = layout.densityCurrent,
          temperature = layout.temperatureCurrent,
          divergence = layout.divergence,
          params = layout.params
        )
      )
      // TODO: Fix SPIR-V FMix error in AdvectionProgram before re-enabling
      // Advection, Diffusion, Projection, Boundary programs temporarily disabled
  
  @main def runFullFluidSimulation(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=== Full Fluid Simulation (All Solver Steps) ===")
      
      // Parameters
      val gridSize = 32
      val totalCells = gridSize * gridSize * gridSize
      val imageSize = (512, 512)
      val numFrames =  10
      
      println(s"Grid: ${gridSize}³ = $totalCells cells")
      println(s"Image: ${imageSize._1}×${imageSize._2}")
      println(s"Frames: $numFrames")
      println()
      
      // Build complete simulation pipeline
      println("Building complete simulation pipeline...")
      val simPipeline = buildPipeline()
      
      // Create renderer
      val renderer = RayMarchRenderer(imageSize._1, imageSize._2)
      
      // Initialize parameters with stronger buoyancy
      val params = FluidParams(
        dt = 0.3f,              // Time step
        viscosity = 0.0001f,    // Low viscosity (smoke is not very viscous)
        diffusion = 0.001f,     // Slight diffusion
        buoyancy = 8.0f,        // Strong buoyancy (hot smoke rises!)
        ambient = 0.0f,         // Ambient temperature
        gridSize = gridSize,
        iterationCount = 20     // Jacobi iterations (don't need 100)
      )
      val paramsBuffer = createParamsBuffer(params)
      
      // Camera setup
      val cameraHeight = -gridSize * 2f
      val lookAtCenter = (gridSize / 2.0f, gridSize / 2.0f, gridSize / 2.0f)
      
      // Allocate GPU memory region
      println("Allocating GPU memory...")
      val region = GBufferRegion
        .allocate[SimulationLayout]
        .map: region =>
          simPipeline.execute(totalCells, region)
      
      // Initialize state buffers
      println("Initializing fluid state...")
      var velocityCurrent = createZeroVec4Buffer(totalCells)
      var velocityPrevious = createZeroVec4Buffer(totalCells)
      var densityCurrent = createZeroFloatBuffer(totalCells)
      var densityPrevious = createZeroFloatBuffer(totalCells)
      var temperatureCurrent = createZeroFloatBuffer(totalCells)
      var temperaturePrevious = createZeroFloatBuffer(totalCells)
      var pressureCurrent = createZeroFloatBuffer(totalCells)
      var pressurePrevious = createZeroFloatBuffer(totalCells)
      val divergence = createZeroFloatBuffer(totalCells)
      
      // Main simulation loop
      for frameIdx <- 0 until numFrames do
        val t = frameIdx.toFloat / numFrames.toFloat
        println(s"\nFrame $frameIdx / $numFrames")
        
        // Add smoke source at bottom center
        println("  Adding smoke source...")
        addSmokeSource(densityCurrent, temperatureCurrent, gridSize, frameIdx)
        
        // Copy current to previous for this timestep
        copyBuffer(velocityCurrent, velocityPrevious)
        copyBuffer(densityCurrent, densityPrevious)
        copyBuffer(temperatureCurrent, temperaturePrevious)
        copyBuffer(pressureCurrent, pressurePrevious)
        
        // Run complete simulation pipeline
        println("  Running simulation pipeline...")
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
            params = GUniform(paramsBuffer)
          ),
          onDone = layout =>
            // Read back updated state
            layout.velocityCurrent.read(velocityCurrent.rewind().asInstanceOf[ByteBuffer])
            layout.densityCurrent.read(densityCurrent.rewind().asInstanceOf[ByteBuffer])
            layout.temperatureCurrent.read(temperatureCurrent.rewind().asInstanceOf[ByteBuffer])
            layout.pressureCurrent.read(pressureCurrent.rewind().asInstanceOf[ByteBuffer])
        )
        
        velocityCurrent.rewind()
        densityCurrent.rewind()
        temperatureCurrent.rewind()
        pressureCurrent.rewind()
        
        // Debug: Check velocity magnitude
        velocityCurrent.rewind()
        var maxVelY = 0.0f
        for i <- 0 until totalCells do
          val vx = velocityCurrent.getFloat()
          val vy = velocityCurrent.getFloat()
          val vz = velocityCurrent.getFloat()
          val vw = velocityCurrent.getFloat()
          if Math.abs(vy) > Math.abs(maxVelY) then maxVelY = vy
        velocityCurrent.rewind()
        println(s"  Max Y velocity: $maxVelY")
        
        // Render current state
        println("  Rendering frame...")
        val angle = 0
        val angleRad = angle * Math.PI.toFloat / 180.0f
        val radius = gridSize  * 1.5f
        
        val camera = Camera3D.orbit(
          centerX = lookAtCenter._1,
          centerY = lookAtCenter._2,
          centerZ = lookAtCenter._3,
          radius = radius,
          angle = angleRad,
          height = cameraHeight,
          aspectRatio = imageSize._1.toFloat / imageSize._2.toFloat
        )
        
        val rendered = renderer.renderFrame(densityCurrent, gridSize, camera)
        val renderedBuffer = ByteBuffer.wrap(rendered).order(ByteOrder.nativeOrder())
        saveFrame(renderedBuffer, imageSize, s"smoke/full_fluid_$frameIdx.png")
        
        println(s"  Saved full_fluid_$frameIdx.png")
      
      println()
      println(s"Done! Created $numFrames frames")
      println("Generate video with: ffmpeg -framerate 10 -i smoke/full_fluid_%d.png -c:v libx264 -pix_fmt yuv420p full_fluid.mp4")
      
    finally
      runtime.close()

  /** Add smoke source at bottom center */
  def addSmokeSource(
    densityBuffer: ByteBuffer,
    temperatureBuffer: ByteBuffer,
    gridSize: Int,
    frameIdx: Int
  ): Unit =
    val pulseFactor = (Math.sin(frameIdx * 0.2).toFloat * 0.3f + 0.7f).max(0.3f)
    val sourceRadius = gridSize / 5
    val centerX = gridSize / 2
    val centerZ = gridSize / 2
    
    // Add density at bottom
    densityBuffer.rewind()
    for z <- 0 until gridSize; y <- 0 until gridSize; x <- 0 until gridSize do
      val idx = x + y * gridSize + z * gridSize * gridSize
      densityBuffer.position(idx * 4)
      
      if y < 4 then
        val dx = x - centerX
        val dz = z - centerZ
        val distSq = dx * dx + dz * dz
        if distSq < sourceRadius * sourceRadius then
          val existing = densityBuffer.getFloat()
          densityBuffer.position(idx * 4)
          val newVal = (existing + 0.6f * pulseFactor).min(1.0f)
          densityBuffer.putFloat(newVal)
    
    // Add temperature
    temperatureBuffer.rewind()
    for z <- 0 until gridSize; y <- 0 until gridSize; x <- 0 until gridSize do
      val idx = x + y * gridSize + z * gridSize * gridSize
      temperatureBuffer.position(idx * 4)
      
      if y < 4 then
        val dx = x - centerX
        val dz = z - centerZ
        val distSq = dx * dx + dz * dz
        if distSq < sourceRadius * sourceRadius then
          val existing = temperatureBuffer.getFloat()
          temperatureBuffer.position(idx * 4)
          val newVal = (existing + 1.5f * pulseFactor).min(3.0f)
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

  /** Save frame as PNG */
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
      image.setRGB(x, y, rgb)
    
    ImageIO.write(image, "png", Paths.get(filename).toFile)

