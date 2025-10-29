package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.core.{GBufferRegion, GCodec, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.fluids.core.GridUtils.trilinearInterpolateFloat32
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import java.nio.ByteOrder

/** Test if density buffer can be sampled correctly */
object DensitySampleTest:
  
  case class Params(gridSize: Int32) extends GStruct[Params]
  
  case class TestLayout(
    output: GBuffer[Vec4[Float32]],
    density: GBuffer[Float32],
    params: GUniform[Params]
  ) extends Layout
  
  @main def testDensitySample(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    val width = 512
    val height = 512
    val gridSize = 32
    
    // Create density field
    val densityData = createSphereDensity(gridSize)
    val densityBuffer = ByteBuffer.allocateDirect(gridSize * gridSize * gridSize * 4)
    densityBuffer.order(ByteOrder.nativeOrder())
    densityData.foreach(densityBuffer.putFloat)
    densityBuffer.flip()
    
    println("Testing density sampling...")
    println(s"Center density (CPU): ${densityData(gridSize/2 + (gridSize/2)*gridSize + (gridSize/2)*gridSize*gridSize)}")
    
    val testProgram = GProgram[Int, TestLayout](
      layout = _ => TestLayout(
        output = GBuffer[Vec4[Float32]](width * height),
        density = GBuffer[Float32](gridSize * gridSize * gridSize),
        params = GUniform[Params]()
      ),
      dispatch = (_, _) => StaticDispatch(((width * height + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1)
    ): layout =>
      val idx = GIO.invocationId
      GIO.when(idx < width * height):
        val params = layout.params.read
        
        // Sample at fixed positions across the volume
        val y = idx / width
        val x = idx.mod(width)
        
        // Map pixel to 3D position in grid
        val px = (x.asFloat / width.toFloat) * params.gridSize.asFloat
        val py = (y.asFloat / height.toFloat) * params.gridSize.asFloat
        val pz = params.gridSize.asFloat / 2.0f  // Fixed Z at middle
        
        val pos = vec3(px, py, pz)
        val density = trilinearInterpolateFloat32(layout.density, pos, params.gridSize)
        
        // Map density to grayscale
        val color = vec4(density, density, density, 1.0f)
        
        for
          _ <- GIO.write(layout.output, idx, color)
        yield Empty()
    
    // Run program
    val region = GBufferRegion
      .allocate[TestLayout]
      .map(l => testProgram.execute(gridSize, l))
    
    val outputBuffer = BufferUtils.createFloatBuffer(width * height * 4)
    val outputBB = org.lwjgl.system.MemoryUtil.memByteBuffer(outputBuffer)
    
    val paramsStride = totalStride(summon[GStructSchema[Params]])
    val paramsBuffer = BufferUtils.createByteBuffer(paramsStride)
    summon[GCodec[Params, Params]].toByteBuffer(paramsBuffer, Array(Params(gridSize)))
    
    densityBuffer.rewind()
    region.runUnsafe(
      init = TestLayout(
        output = GBuffer[Vec4[Float32]](width * height),
        density = GBuffer[Float32](densityBuffer),
        params = GUniform[Params](paramsBuffer)
      ),
      onDone = layout => layout.output.read(outputBB)
    )
    
    val outputData = Array.ofDim[Float](width * height * 4)
    outputBuffer.rewind()
    outputBuffer.get(outputData)
    
    // Check center pixel
    val centerIdx = (height/2 * width + width/2) * 4
    val centerDensity = outputData(centerIdx)
    println(s"Center density (GPU): $centerDensity")
    
    println("Done")
    runtime.close()
  
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

