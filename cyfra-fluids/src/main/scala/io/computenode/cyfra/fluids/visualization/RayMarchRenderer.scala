package io.computenode.cyfra.fluids.visualization

import io.computenode.cyfra.core.{GBufferRegion, GCodec, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.dsl.control.When.when
import io.computenode.cyfra.dsl.library.Functions.{abs, clamp, max, min, mix, tan}
import io.computenode.cyfra.dsl.library.Math3D.*
import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.fluids.core.GridUtils
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import java.nio.ByteOrder

/** Volume ray marching renderer for fluid visualization.
  * 
  * Renders 3D fluid density using ray marching with opacity accumulation.
  * 
  * @param width Image width in pixels
  * @param height Image height in pixels
  * @param runtime Cyfra runtime for GPU execution
  */
class RayMarchRenderer(width: Int, height: Int)(using runtime: VkCyfraRuntime):
  
  import GridUtils.*
  
  /** Ray marching state for accumulation along ray */
  private case class RayMarchState(
    pos: Vec3[Float32],
    accumColor: Vec4[Float32],
    transmittance: Float32,
    t: Float32,
    finished: GBoolean = false
  ) extends GStruct[RayMarchState]
  
  /** Parameters for rendering */
  case class RenderParams(
    gridSize: Int32
  ) extends GStruct[RenderParams]
  
  /** Layout for rendering. */
  case class RenderLayout(
    output: GBuffer[Vec4[Float32]],
    density: GBuffer[Float32],
    camera: GUniform[Camera3D],
    params: GUniform[RenderParams]
  ) extends Layout
  
  /** Compute ray-box intersection and return entry distance.
    * Returns -1 if no intersection.
    */
  private def rayBoxIntersection(
    origin: Vec3[Float32],
    direction: Vec3[Float32],
    boxMin: Vec3[Float32],
    boxMax: Vec3[Float32]
  ): Float32 =
    // Compute inverse direction (infinity/NaN handled by min/max)
    val invDirX = 1.0f / direction.x
    val invDirY = 1.0f / direction.y
    val invDirZ = 1.0f / direction.z
    
    // Compute intersection t values for each slab
    val t1x = (boxMin.x - origin.x) * invDirX
    val t2x = (boxMax.x - origin.x) * invDirX
    val t1y = (boxMin.y - origin.y) * invDirY
    val t2y = (boxMax.y - origin.y) * invDirY
    val t1z = (boxMin.z - origin.z) * invDirZ
    val t2z = (boxMax.z - origin.z) * invDirZ
    
    // Find min/max for each axis
    val tminx = min(t1x, t2x)
    val tmaxx = max(t1x, t2x)
    val tminy = min(t1y, t2y)
    val tmaxy = max(t1y, t2y)
    val tminz = min(t1z, t2z)
    val tmaxz = max(t1z, t2z)
    
    // Find overall entry and exit distances
    val tNear = max(max(tminx, tminy), tminz)
    val tFar = min(min(tmaxx, tmaxy), tmaxz)
    
    // Check for valid intersection
    when((tFar < 0.0f) || (tNear > tFar)):
      -1.0f  // No intersection
    .otherwise:
      max(tNear, 0.0f)  // Entry distance (clamp to 0 if inside box)
  
  /** March along a ray and accumulate color/opacity */
  private def marchRay(
    origin: Vec3[Float32],
    direction: Vec3[Float32],
    densityBuffer: GBuffer[Float32],
    gridSize: Int32
  ): Vec4[Float32] =
    val maxSteps = 512
    val stepSize = 0.15f
    val absorptionCoeff = 0.3f
    val transmittanceThreshold = 0.01f
    
    // Define grid bounding box
    val boxMin = vec3(0.0f, 0.0f, 0.0f)
    val boxMax = vec3(gridSize.asFloat, gridSize.asFloat, gridSize.asFloat)
    
    // Find where ray enters the volume
    val entryDist = rayBoxIntersection(origin, direction, boxMin, boxMax)
    
    // Background color
    val backgroundColor = vec4(0.05f, 0.05f, 0.1f, 1.0f)
    
    // If ray doesn't intersect volume, return background
    when(entryDist < 0.0f):
      backgroundColor
    .otherwise:
      // Start marching from entry point
      val startPos = origin + direction * entryDist
      val maxDistance = gridSize.asFloat * 3.0f
      
      val initState = RayMarchState(
        pos = startPos,
        accumColor = vec4(0.0f, 0.0f, 0.0f, 0.0f),
        transmittance = 1.0f,
        t = entryDist,
        finished = false
      )
      
      val finalState = GSeq
        .gen[RayMarchState](
          first = initState,
          next = state =>
            val nextPos = state.pos + direction * stepSize
            val nextT = state.t + stepSize
            val density = trilinearInterpolateFloat32(densityBuffer, state.pos, gridSize)
            
            // Map density to color (heat map)
            val sampleColor = FluidColorMap.heatMap(density)
            
            // Calculate opacity from density
            val alpha = clamp(density * absorptionCoeff, 0.0f, 1.0f)
            
            // Accumulate color with current transmittance
            val colorContribution = sampleColor * alpha * state.transmittance
            val nextColor = state.accumColor + colorContribution
            
            // Update transmittance
            val nextTransmittance = state.transmittance * (1.0f - alpha)
            
            // Check if ray should terminate
            val shouldFinish = (nextTransmittance < transmittanceThreshold) || (nextT > maxDistance)
            RayMarchState(nextPos, nextColor, nextTransmittance, nextT, shouldFinish)
        )
        .limit(maxSteps)
        .takeWhile(!_.finished)
        .lastOr(initState)
      
      // Add background with remaining transmittance
      finalState.accumColor + backgroundColor * finalState.transmittance
  
  /** Generate ray direction from camera through pixel (returns just direction) */
  private def generateRayDirection(
    x: Int32,
    y: Int32,
    camera: Camera3D
  ): Vec3[Float32] =
    // Normalize pixel coordinates to [-1, 1]
    val aspectRatio = width.toFloat / height.toFloat
    val u = (x.asFloat / width.toFloat) * 2.0f - 1.0f
    val v = ((y.asFloat / height.toFloat) * 2.0f - 1.0f) / aspectRatio
    
    // Extract Vec3 from Vec4 camera vectors (ignore w component)
    val pos3 = vec3(camera.position.x, camera.position.y, camera.position.z)
    val target3 = vec3(camera.target.x, camera.target.y, camera.target.z)
    val up3 = vec3(camera.up.x, camera.up.y, camera.up.z)
    
    // Camera basis vectors
    val forward = normalize(target3 - pos3)
    val right = normalize(cross(forward, up3))
    val up = cross(right, forward)
    
    // Calculate ray direction based on FOV
    val fovRad = camera.fov * 3.14159265f / 180.0f
    val cameraDist = 1.0f / tan(fovRad * 0.5f)
    val rayTarget = (right * u) + (up * v) + (forward * cameraDist)
    normalize[Vec3[Float32]](rayTarget)
  
  /** Rendering program */
  private val renderProgram = GProgram[Int, RenderLayout](
    layout = gridSize => {
      import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
      val totalCells = gridSize * gridSize * gridSize
      RenderLayout(
        output = GBuffer[Vec4[Float32]](width * height),
        density = GBuffer[Float32](totalCells),  // Matches the density buffer size
        camera = GUniform[Camera3D](),
        params = GUniform[RenderParams]()
      )
    },
    dispatch = (layout, _) => {
      val totalPixels = width * height
      val workgroupSize = 256
      val numWorkgroups = (totalPixels + workgroupSize - 1) / workgroupSize
      StaticDispatch((numWorkgroups, 1, 1))
    },
    workgroupSize = (256, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val totalPixels = width * height
    
    GIO.when(idx < totalPixels):
      // Convert 1D index to 2D pixel coordinates
      val y = idx / width
      val x = idx.mod(width)
      
      // Read camera and params
      val cam = layout.camera.read
      val params = layout.params.read
      
      // Generate ray for this pixel
      val rayDirection = generateRayDirection(x, y, cam)
      
      // Extract camera position as Vec3 (ignore w)
      val camPos = vec3(cam.position.x, cam.position.y, cam.position.z)
      
      // March along ray and accumulate color
      val finalColor = marchRay(camPos, rayDirection, layout.density, params.gridSize)
      
      for
        _ <- GIO.write(layout.output, idx, finalColor)
      yield Empty()
  
  /** Render a frame of the fluid simulation.
    * 
    * @param densityBuffer Fluid density buffer (must be gridSize³ elements)
    * @param gridSize Grid resolution
    * @param camera Camera parameters
    * @return RGBA image data as byte array (width × height × 4)
    */
  def renderFrame(
    densityBuffer: ByteBuffer,
    gridSize: Int,
    camera: Camera3D
  ): Array[Byte] =
    val region = GBufferRegion
      .allocate[RenderLayout]
      .map: layout =>
        renderProgram.execute(gridSize, layout)
    
    // Prepare output buffer for reading back
    val outputData = Array.ofDim[Float](width * height * 4)
    val outputBuffer = BufferUtils.createFloatBuffer(width * height * 4)
    val outputBB = org.lwjgl.system.MemoryUtil.memByteBuffer(outputBuffer)
    
    // Prepare camera buffer
    val cameraStride = totalStride(summon[GStructSchema[Camera3D]])
    val cameraBuffer = BufferUtils.createByteBuffer(cameraStride)
    summon[GCodec[Camera3D, Camera3D]].toByteBuffer(cameraBuffer, Array(camera))
    
    // Prepare params buffer
    val paramsStride = totalStride(summon[GStructSchema[RenderParams]])
    val paramsBuffer = BufferUtils.createByteBuffer(paramsStride)
    val renderParams = RenderParams(gridSize = gridSize)
    summon[GCodec[RenderParams, RenderParams]].toByteBuffer(paramsBuffer, Array(renderParams))
    
    // Execute GPU program
    region.runUnsafe(
      init = RenderLayout(
        output = GBuffer[Vec4[Float32]](width * height),
        density = GBuffer[Float32](densityBuffer),
        camera = GUniform[Camera3D](cameraBuffer),
        params = GUniform[RenderParams](paramsBuffer)
      ),
      onDone = layout => layout.output.read(outputBB)
    )
    
    // Copy from buffer to array
    outputBuffer.rewind()
    outputBuffer.get(outputData)
    
    // Convert float colors to bytes (0-255)
    outputData.map(f => (clampFloat(f, 0.0f, 1.0f) * 255.0f).toByte)
    

  /** Helper to clamp float values (CPU version) */
  private def clampFloat(value: Float, min: Float, max: Float): Float =
    if value < min then min
    else if value > max then max
    else value
