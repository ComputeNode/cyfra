package io.computenode.cyfra.fluids.runner

import io.computenode.cyfra.core.{GBufferRegion, GCodec, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.*
import io.computenode.cyfra.fluids.solver.programs.*
import io.computenode.cyfra.fluids.solver.utils.FieldUtils
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
      .addProgram(ForcesProgram.create)(
        totalCells => totalCells,
        _.toFluidStatePrevious
      )
      .addProgram(VorticityConfinementProgram.create)(
        totalCells => totalCells,
        _.toFluidStatePrevious
      )
      .addProgram(AdvectionProgram.create)(
        totalCells => totalCells,
        _.toFluidStateDouble
      )
      .addProgram(DiffusionProgram.create)(
        totalCells => totalCells,
        _.toFluidStateDouble
      )
      .addProgram(ProjectionProgram.divergence)(
        totalCells => totalCells,
        _.toFluidStateCurrent
      )
      .addProgram(ProjectionProgram.pressureSolve)(
        totalCells => totalCells,
        _.toFluidStateDouble
      )
      .pipe: ex =>
        LazyList.iterate(ex): nex =>
          nex.addProgram(ProjectionProgram.pressureSolve)(totalCells => totalCells, _.toFluidStateDouble)
            .addProgram(ProjectionProgram.pressureSolve)(totalCells => totalCells, _.toFluidStateDoubleSwap)
        .apply(jacobiIters)
      .addProgram(ProjectionProgram.subtractGradient)(
        totalCells => totalCells,
        _.toFluidStateCurrent
      )
      .addProgram(OutflowBoundaryProgram.create)(
        totalCells => totalCells,
        _.toFluidStateCurrent
      )
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
      
      val gridSize = 128
      val totalCells = gridSize * gridSize * gridSize
      val imageSize = (512, 512)
      val numFrames = 1000
      
      logger.info(s"Grid: ${gridSize}³ = $totalCells cells")
      logger.info(s"Image: ${imageSize._1}×${imageSize._2}")
      logger.info(s"Frames: $numFrames")
      
      logger.info("Building complete simulation pipeline...")
      val simPipeline = buildPipeline(imageSize, 64)

      val params = FluidParams(
        dt = 0.05f,
        viscosity = 0.001f,
        diffusion = 0.00001f,
        buoyancy = 0.001f,
        ambient = 0.005f,
        gridSize = gridSize,
        windX = 0f,
        windY = 0.0f,
        windZ = 0f
      )
      val paramsBuffer = createParamsBuffer(params)
      
      val cameraHeight = gridSize * 0f
      val lookAtCenter = (gridSize / 2.0f, gridSize / 2.0f, gridSize / 2.0f)
      
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

      val center = gridSize / 2.0f

      FieldUtils.addSpiralObstacle(
        obstacles,
        gridSize,
        centerX = center,
        centerY = gridSize * 0.4f,
        centerZ = center,
        spiralRadius = gridSize * 0.1f,
        spiralHeight = gridSize * 0.3f,
        numTurns = 2.5f,
        ribbonWidth = gridSize * 0.08f,
        ribbonThickness = gridSize * 0.03f,
        ribbonHeight = gridSize * 0.07f,
        rotationDegrees = 180f,
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

            FieldUtils.addCollidingDiscCurrents(velocityPrevious, dyePrevious, gridSize, frameIdx)
    
            val outputBuffer = BufferUtils.createFloatBuffer(imageSize._1 * imageSize._2 * 4)
            val outputBB = org.lwjgl.system.MemoryUtil.memByteBuffer(outputBuffer)
            
            val angle = 90
            val angleRad = angle * Math.PI.toFloat / 180.0f
            val radius = gridSize * 1.4f
    
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
    
            val renderParams = RenderParams(gridSize = gridSize)
            summon[GCodec[RenderParams, RenderParams]].toByteBuffer(renderParamsBuffer, Array(renderParams))

            layout.velocityPrevious.write(velocityPrevious)
            layout.dyePrevious.write(dyePrevious)
            layout.renderParams.write(renderParamsBuffer)
            layout.camera.write(cameraBuffer)

            logger.debug("Running simulation pipeline...")
            simPipeline.execute(totalCells, layout)
            
            layout.imageOutput.read(outputBB)
    
            velocityPrevious.rewind()
            dyePrevious.rewind()
    
            outputBuffer.rewind()
            
            val outputPixels = ByteBuffer.allocateDirect(imageSize._1 * imageSize._2 * 4)
            while outputBuffer.hasRemaining do
              val pixel = (Math.clamp(outputBuffer.get(), 0.0f, 1.0f) * 255).toByte
              outputPixels.put(pixel)
    
            saveFrame(outputPixels, (imageSize._1, imageSize._2), s"smoke/full_fluid_$frameIdx.png")
    
            logger.debug(s"Saved full_fluid_$frameIdx.png")
    
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
      image.setRGB(x, height - 1 - y, rgb)
    
    val file = Paths.get(filename).toFile
    Option(file.getParentFile).foreach(_.mkdirs())
    ImageIO.write(image, "png", file)
