package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.ObstacleUtils
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fluids.visualization.{Camera3D, RayMarchRenderer}
import io.computenode.cyfra.spirvtools.SpirvTool.ToFile
import io.computenode.cyfra.spirvtools.{SpirvCross, SpirvDisassembler, SpirvToolsRunner}
import io.computenode.cyfra.utility.Logger.logger

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.nio.file.Paths

/** Demo of volume ray marching visualization for fluid simulation.
  * 
  * Creates a simple test density field and renders it from multiple angles.
  */
object RayMarchVisualizationDemo:
  
  @main def rayMarchVisualization(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime(spirvToolsRunner =
      SpirvToolsRunner(
        // crossCompilation = SpirvCross.Enable(toolOutput = ToFile(Paths.get("output/failing.glsl")), throwOnFail = true),
        disassembler = SpirvDisassembler.Enable(toolOutput = ToFile(Paths.get("output/failingg.spvdis"))),
      ),
    )
    
    logger.info("=== Ray March Visualization Demo ===")
    
    // Grid parameters
    val gridSize = 32
    val totalCells = gridSize * gridSize * gridSize
    
    // Image parameters
    val imageWidth = 512
    val imageHeight = 512
    val aspectRatio = imageWidth.toFloat / imageHeight.toFloat
    
    logger.info(s"Grid: ${gridSize}³ = $totalCells cells")
    logger.info(s"Image: ${imageWidth}×${imageHeight}")
    
    // Create test density field: sphere in center
    val densityData = createSphereDensity(gridSize)
    
    // Convert to ByteBuffer
    val densityBuffer = ByteBuffer.allocateDirect(totalCells * 4)
    densityBuffer.order(ByteOrder.nativeOrder())
    densityData.foreach(densityBuffer.putFloat)
    densityBuffer.flip()
    
    // Create renderer
    val renderer = RayMarchRenderer(imageWidth, imageHeight)
    
    // Render from different angles
    val numFrames = 8
    for frame <- 0 `until` numFrames do
      val angle = (frame.toFloat / numFrames.toFloat) * 2.0f * Math.PI.toFloat / 4
      val centerPos = gridSize.toFloat / 2.0f
      
      // Debug: print raw calculation values
      val radius = gridSize.toFloat * 3f
      val cosAngle = Math.cos(angle).toFloat
      val sinAngle = Math.sin(angle).toFloat
      val camX = centerPos + radius * cosAngle
      val camZ = centerPos + radius * sinAngle
      
      logger.info(s"Rendering frame $frame (angle = ${Math.toDegrees(angle).toInt}°)...")
      logger.debug(s"Raw: cos=$cosAngle, sin=$sinAngle")
      logger.debug(s"Computed position: x=$camX, z=$camZ")
      
      val camera = Camera3D.orbit(
        centerX = centerPos,
        centerY = centerPos,
        centerZ = centerPos,
        radius = radius,
        angle = angle,
        height = gridSize.toFloat * 0.3f,
        aspectRatio = aspectRatio
      )
      
      logger.debug(s"Camera.position fields: ${camera.position}")
      
      // Create empty obstacles buffer for this demo (no obstacles)
      val obstaclesBuffer = ObstacleUtils.createEmpty(gridSize)
      ObstacleUtils.addSphere(obstaclesBuffer, gridSize, gridSize / 2.0f, gridSize / 2.0f, gridSize / 2.0f, gridSize / 3.0f, 0.8f)
      // test if buffer is correct
             
      
      densityBuffer.rewind()  // Reset buffer position for reuse
      val imageData = renderer.renderFrame(densityBuffer, obstaclesBuffer, gridSize, camera)
      
      // Save image
      saveImage(imageData, imageWidth, imageHeight, s"raymarch_frame_$frame.png")
      
      logger.debug(s"Saved raymarch_frame_$frame.png")
    
    logger.info(s"Done! Created $numFrames frames")
    summon[VkCyfraRuntime].close()
  
  /** Create a test density field with a sphere in the center.
    * 
    * @param gridSize Grid resolution
    * @return Array of density values
    */
  private def createSphereDensity(gridSize: Int): Array[Float] =
    val center = gridSize / 2.0f
    val radius = gridSize / 4.0f
    
    val data = Array.ofDim[Float](gridSize * gridSize * gridSize)
    
    for
      z <- 0 `until` gridSize
      y <- 0 `until` gridSize
      x <- 0 `until` gridSize
    do
      val idx = x + y * gridSize + z * gridSize * gridSize
      
      val dx = x.toFloat - center
      val dy = y.toFloat - center
      val dz = z.toFloat - center
      val dist = Math.sqrt(dx*dx + dy*dy + dz*dz).toFloat
      
      // Smooth sphere with falloff
      val density = if dist < radius then
        val t = dist / radius
        1.0f - t * t  // Quadratic falloff
      else
        0.0f
      
      data(idx) = density
    
    logger.info(s"Created sphere density field: radius=$radius, center=$center")
    
    // Print some statistics
    val nonZero = data.count(_ > 0.0f)
    val maxDensity = data.max
    logger.info(s"Non-zero cells: $nonZero / ${data.length} (${100*nonZero/data.length}%)")
    logger.info(s"Max density: $maxDensity")
    
    data
  
  /** Save RGBA image data to PNG file.
    * 
    * @param data RGBA byte array
    * @param width Image width
    * @param height Image height
    * @param filename Output filename
    */
  private def saveImage(data: Array[Byte], width: Int, height: Int, filename: String): Unit =
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    
    for
      y <- 0 `until` height
      x <- 0 `until` width
    do
      val idx = (y * width + x) * 4
      val r = (data(idx + 0) & 0xFF)
      val g = (data(idx + 1) & 0xFF)
      val b = (data(idx + 2) & 0xFF)
      val a = (data(idx + 3) & 0xFF)
      val argb = (a << 24) | (r << 16) | (g << 8) | b
      image.setRGB(x, y, argb)
    
    ImageIO.write(image, "png", File(filename))
