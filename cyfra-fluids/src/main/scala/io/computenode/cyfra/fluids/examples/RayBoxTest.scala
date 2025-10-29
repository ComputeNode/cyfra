package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fluids.visualization.Camera3D
import io.computenode.cyfra.core.{GBufferRegion, GCodec, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.library.Functions.{max, min, tan}
import io.computenode.cyfra.dsl.library.Math3D.*
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

/** Minimal test to isolate ray-box intersection issue */
object RayBoxTest:
  
  case class TestLayout(
    output: GBuffer[Vec4[Float32]],
    camera: GUniform[Camera3D]
  ) extends Layout
  
  @main def testRayBox(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    val width = 512
    val height = 512
    val gridSize = 32
    val aspectRatio = width.toFloat / height.toFloat
    
    // Test two angles: 0° (works) and 45° (fails)
    val angles = Array(0.0f, Math.PI.toFloat / 4.0f)
    val labels = Array("0deg", "45deg")
    
    for i <- 0 `until` angles.length do
      val angle = angles(i)
      val label = labels(i)
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
      
      println(s"\n=== Testing angle $label ===")
      println(s"Camera position: (${camera.position})")
      
      val testProgram = GProgram[Int, TestLayout](
        layout = _ => TestLayout(
          output = GBuffer[Vec4[Float32]](width * height),
          camera = GUniform[Camera3D]()
        ),
        dispatch = (_, _) => StaticDispatch(((width * height + 255) / 256, 1, 1)),
        workgroupSize = (256, 1, 1)
      ): layout =>
        val idx = GIO.invocationId
        GIO.when(idx < width * height):
          val y = idx / width
          val x = idx.mod(width)
          val cam = layout.camera.read
          
          // Generate ray
          val u = (x.asFloat / width.toFloat) * 2.0f - 1.0f
          val v = ((y.asFloat / height.toFloat) * 2.0f - 1.0f) / aspectRatio
          
          val pos3 = vec3(cam.position.x, cam.position.y, cam.position.z)
          val target3 = vec3(cam.target.x, cam.target.y, cam.target.z)
          val up3 = vec3(cam.up.x, cam.up.y, cam.up.z)
          
          val forward = normalize(target3 - pos3)
          val right = normalize(cross(forward, up3))
          val up = cross(right, forward)
          
          val fovRad = cam.fov * 3.14159265f / 180.0f
          val cameraDist = 1.0f / tan(fovRad * 0.5f)
          val rayTarget = (right * u) + (up * v) + (forward * cameraDist)
          val rayDir = normalize[Vec3[Float32]](rayTarget)
          
          // Ray-box intersection
          val boxMin = vec3(0.0f, 0.0f, 0.0f)
          val boxMax = vec3(gridSize.toFloat, gridSize.toFloat, gridSize.toFloat)
          
          val invDirX = 1.0f / rayDir.x
          val invDirY = 1.0f / rayDir.y
          val invDirZ = 1.0f / rayDir.z
          
          val t1x = (boxMin.x - pos3.x) * invDirX
          val t2x = (boxMax.x - pos3.x) * invDirX
          val t1y = (boxMin.y - pos3.y) * invDirY
          val t2y = (boxMax.y - pos3.y) * invDirY
          val t1z = (boxMin.z - pos3.z) * invDirZ
          val t2z = (boxMax.z - pos3.z) * invDirZ
          
          val tminx = min(t1x, t2x)
          val tmaxx = max(t1x, t2x)
          val tminy = min(t1y, t2y)
          val tmaxy = max(t1y, t2y)
          val tminz = min(t1z, t2z)
          val tmaxz = max(t1z, t2z)
          
          val tNear = max(max(tminx, tminy), tminz)
          val tFar = min(min(tmaxx, tmaxy), tmaxz)
          
          // Simple color: red if hit, blue if miss
          val color = when((tFar < 0.0f) || (tNear > tFar)):
            vec4(0.0f, 0.0f, 1.0f, 1.0f)  // Blue = miss
          .otherwise:
            vec4(1.0f, 0.0f, 0.0f, 1.0f)  // Red = hit
          
          for
            _ <- GIO.write(layout.output, idx, color)
          yield Empty()
      
      // Run program
      val region = GBufferRegion
        .allocate[TestLayout]
        .map(l => testProgram.execute(gridSize, l))
      
      val outputBuffer = BufferUtils.createFloatBuffer(width * height * 4)
      val outputBB = org.lwjgl.system.MemoryUtil.memByteBuffer(outputBuffer)
      
      val cameraStride = totalStride(summon[GStructSchema[Camera3D]])
      val cameraBuffer = BufferUtils.createByteBuffer(cameraStride)
      summon[GCodec[Camera3D, Camera3D]].toByteBuffer(cameraBuffer, Array(camera))
      
      region.runUnsafe(
        init = TestLayout(
          output = GBuffer[Vec4[Float32]](width * height),
          camera = GUniform[Camera3D](cameraBuffer)
        ),
        onDone = layout => layout.output.read(outputBB)
      )
      
      val outputData = Array.ofDim[Float](width * height * 4)
      outputBuffer.rewind()
      outputBuffer.get(outputData)
      
      // Convert to bytes and save
      val imageData = outputData.map(f => (f.min(1.0f).max(0.0f) * 255.0f).toByte)
      saveImage(imageData, width, height, s"ray_box_test_$label.png")
      
      println(s"Saved ray_box_test_$label.png")
    
    runtime.close()
  
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

