package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fluids.visualization.{Camera3D, RayMarchRenderer}
import java.nio.ByteBuffer
import java.nio.ByteOrder

/** Debug version that prints GPU values for a single problem angle */
object RayMarchDebug:
  
  @main def rayMarchDebug(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    println("=== Ray March Debug (45° angle) ===")
    
    val gridSize = 32
    val totalCells = gridSize * gridSize * gridSize
    val imageWidth = 512
    val imageHeight = 512
    val aspectRatio = imageWidth.toFloat / imageHeight.toFloat
    
    // Create test density field
    val densityData = createSphereDensity(gridSize)
    val densityBuffer = ByteBuffer.allocateDirect(totalCells * 4)
    densityBuffer.order(ByteOrder.nativeOrder())
    densityData.foreach(densityBuffer.putFloat)
    densityBuffer.flip()
    
    // Test angle that fails: 45 degrees
    val angle = Math.PI.toFloat / 4.0f  // 45 degrees
    val centerPos = gridSize.toFloat / 2.0f
    val radius = gridSize.toFloat * 1.5f
    
    val camera = Camera3D.orbit(
      centerX = centerPos,
      centerY = centerPos,
      centerZ = centerPos,
      radius = radius,
      angle = angle,
      height = gridSize.toFloat * 0.3f,
      aspectRatio = aspectRatio
    )
    
    println(s"Camera position: (${camera.position})")
    println(s"Camera target: (${camera.target})")
    println(s"Grid bounds: [0, 0, 0] to [$gridSize, $gridSize, $gridSize]")
    
    // Create renderer
    val renderer = RayMarchRenderer(imageWidth, imageHeight)
    densityBuffer.rewind()
    val imageData = renderer.renderFrame(densityBuffer, gridSize, camera)
    
    println("Done - check console for GPU printf output")
    summon[VkCyfraRuntime].close()
  
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
      val density = if dist < radius then 1.0f - (dist / radius) * (dist / radius) else 0.0f
      data(idx) = density
    
    data

