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
import io.computenode.cyfra.fluids.solver.GridUtils
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import java.nio.ByteOrder
import RayMarchRenderer.*
import GridUtils.*

/** Volume ray marching renderer for fluid visualization.
  *
  * Renders 3D fluid density using ray marching with opacity accumulation.
  *
  * @param width
  *   Image width in pixels
  * @param height
  *   Image height in pixels
  * @param runtime
  *   Cyfra runtime for GPU execution
  */
class RayMarchRenderer(rendererConfig: RendererConfig):

  private val width = rendererConfig.width
  private val height = rendererConfig.height

  /** Compute ray-box intersection and return entry distance. Returns -1 if no intersection.
    */
  private def rayBoxIntersection(origin: Vec3[Float32], direction: Vec3[Float32], boxMin: Vec3[Float32], boxMax: Vec3[Float32]): Float32 =
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
      -1.0f // No intersection
    .otherwise:
      max(tNear, 0.0f) // Entry distance (clamp to 0 if inside box)

  /** March along a ray and accumulate color/opacity */
  private def marchRay(
    origin: Vec3[Float32],
    direction: Vec3[Float32],
    velocityBuffer: GBuffer[Vec4[Float32]],
    pressureBuffer: GBuffer[Float32],
    densityBuffer: GBuffer[Float32],
    temperatureBuffer: GBuffer[Float32],
    obstaclesBuffer: GBuffer[Float32],
    gridSize: Int32,
  ): Vec4[Float32] =
    val maxSteps = 2048
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

      val initState = RayMarchState(pos = startPos, accumColor = vec4(0.0f, 0.0f, 0.0f, 0.0f), transmittance = 1.0f, t = entryDist)

      val finalState = GSeq
        .gen[RayMarchState](
          first = initState,
          next = state =>
            import io.computenode.cyfra.fluids.solver.ObstacleUtils

            val nextPos = state.pos + direction * stepSize
            val nextT = state.t + stepSize

            // Check if ray is still inside the grid volume (continuous bounds check)
            val gridMax = gridSize.asFloat
            val posInBounds =
              (state.pos.x >= 0.0f) &&
                (state.pos.x <= gridMax) &&
                (state.pos.y >= 0.0f) &&
                (state.pos.y <= gridMax) &&
                (state.pos.z >= 0.0f) &&
                (state.pos.z <= gridMax)

            // Check if next position will be outside (to stop BEFORE exiting)
            val nextPosInBounds =
              (nextPos.x >= 0.0f) &&
                (nextPos.x <= gridMax) &&
                (nextPos.y >= 0.0f) &&
                (nextPos.y <= gridMax) &&
                (nextPos.z >= 0.0f) &&
                (nextPos.z <= gridMax)

            // Check if we're inside an obstacle (only if in bounds)
            val cellX = state.pos.x.asInt
            val cellY = state.pos.y.asInt
            val cellZ = state.pos.z.asInt
            val cellInBounds =
              (cellX >= 0) &&
                (cellX < gridSize) &&
                (cellY >= 0) &&
                (cellY < gridSize) &&
                (cellZ >= 0) &&
                (cellZ < gridSize)
            val cellIdx = coord3dToIdx(cellX, cellY, cellZ, gridSize)
            val totalCells = gridSize * gridSize * gridSize
            val hitObstacle = cellInBounds && ObstacleUtils.isSolid(obstaclesBuffer, cellIdx, totalCells)

            when(hitObstacle && rendererConfig.renderObstacles):
              // Get obstacle color/brightness value
              val obstacleValue = ObstacleUtils.getObstacleValue(obstaclesBuffer, cellIdx)

              // Simple ambient + diffuse lighting for obstacle
              val lightDir = normalize(vec3(-1f, -1f, -1f))
              val normal = ObstacleUtils.computeNormal(obstaclesBuffer, cellX, cellY, cellZ, gridSize)
              val diffuse = max(0.0f, normal dot lightDir)
              val ambient = 0.3f
              val brightness = clamp(ambient + diffuse * 0.7f, 0.0f, 1.0f)

              val sampleColor = vec4(brightness * 1f, brightness * 0.3f, brightness * 0.3f * 1.1f, 1.0f)

              val colorContribution = sampleColor  * state.transmittance
              val nextColor = state.accumColor + colorContribution

              val nextState =
                when(state.state === MarchState.LAST_STEP)(MarchState.DONE)
                  .otherwise(MarchState.LAST_STEP)
              RayMarchState(nextPos, nextColor, 0f, nextT, nextState)
            .otherwise:
              val rescaleFactor = rendererConfig.renderMax - rendererConfig.renderOver
              val (sampleColor, opacity) = rendererConfig.fieldToRender match
                case Field.Density =>
                  val density = trilinearInterpolateFloat32(densityBuffer, state.pos, gridSize)
                  val clamped = clamp(density - rendererConfig.renderOver, 0.0f, rendererConfig.renderMax) / rescaleFactor
                  (FluidColorMap.heatMap(clamped), clamped)
                case Field.Pressure =>
                  val pressure = trilinearInterpolateFloat32(pressureBuffer, state.pos, gridSize)
                  val clamped = clamp(pressure - rendererConfig.renderOver, 0.0f, rendererConfig.renderMax) / rescaleFactor
                  (FluidColorMap.heatMap(clamped), clamped)
                case Field.Temperature =>
                  val temperature = trilinearInterpolateFloat32(temperatureBuffer, state.pos, gridSize)
                  val clamped = clamp(temperature - rendererConfig.renderOver, 0.0f, rendererConfig.renderMax) / rescaleFactor
                  (FluidColorMap.heatMap(clamped), clamped)
                case Field.Velocity =>
                  val velocity = abs(trilinearInterpolateVec4(velocityBuffer, state.pos, gridSize))
                  val velLen = length(velocity.xyz)
                  val clamped = clamp(velLen - rendererConfig.renderOver, 0.0f, rendererConfig.renderMax) / rescaleFactor
                  (FluidColorMap.heatMap(clamped), clamped)

              val alpha = clamp(opacity * absorptionCoeff, 0.0f, 1.0f)

              // Accumulate color with current transmittance
              val colorContribution = sampleColor * alpha * state.transmittance
              val nextColor = state.accumColor + colorContribution

              // Update transmittance
              val nextTransmittance = state.transmittance * (1.0f - alpha)

              // Check if ray should terminate (stop if next step would exit volume)
              val shouldFinish = (!nextPosInBounds) || (nextTransmittance < transmittanceThreshold) || (nextT > maxDistance)
              val nextState =
                when(state.state === MarchState.LAST_STEP)(MarchState.DONE)
                  .elseWhen(hitObstacle)(MarchState.LAST_STEP)
                  .elseWhen(shouldFinish)(MarchState.DONE)
                  .otherwise(MarchState.MARCHING)
              RayMarchState(nextPos, nextColor, nextTransmittance, nextT, nextState),

        )
        .limit(maxSteps)
        .takeWhile(_.state !== MarchState.DONE)
        .lastOr(initState)

      // Add background with remaining transmittance
      finalState.accumColor + backgroundColor * finalState.transmittance

  /** Generate ray direction from camera through pixel (returns just direction) */
  private def generateRayDirection(x: Int32, y: Int32, camera: Camera3D): Vec3[Float32] =
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
  val renderProgram = GProgram[Int, RenderLayout](
    layout = totalCells => {
      import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
      RenderLayout(
        output = GBuffer[Vec4[Float32]](width * height),
        velocity = GBuffer[Vec4[Float32]](totalCells),
        pressure = GBuffer[Float32](totalCells),
        density = GBuffer[Float32](totalCells),
        temperature = GBuffer[Float32](totalCells),
        obstacles = GBuffer[Float32](totalCells),
        camera = GUniform[Camera3D](),
        params = GUniform[RenderParams](),
      )
    },
    dispatch = (layout, _) => {
      val totalPixels = width * height
      val workgroupSize = 256
      val numWorkgroups = (totalPixels + workgroupSize - 1) / workgroupSize
      StaticDispatch((numWorkgroups, 1, 1))
    },
    workgroupSize = (256, 1, 1),
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
      val finalColor = marchRay(camPos, rayDirection, layout.velocity, layout.pressure, layout.density, layout.temperature, layout.obstacles, params.gridSize)

      for _ <- GIO.write(layout.output, idx, finalColor)
      yield Empty()

  /** Helper to clamp float values (CPU version) */
  private def clampFloat(value: Float, min: Float, max: Float): Float =
    if value < min then min
    else if value > max then max
    else value

object RayMarchRenderer:

  case class RendererConfig(
    width: Int, height: Int,
    fieldToRender: Field,
    renderOver: Float = 0f,
    renderMax: Float = 1f,
    renderObstacles: Boolean = true
  )

  enum Field:
    case Velocity
    case Pressure
    case Density
    case Temperature


  object MarchState:
    val MARCHING: Int32 = 0
    val LAST_STEP: Int32 = 1
    val DONE: Int32 = 2

  /** Ray marching state for accumulation along ray */
  private case class RayMarchState(
    pos: Vec3[Float32],
    accumColor: Vec4[Float32],
    transmittance: Float32,
    t: Float32,
    state: Int32 = MarchState.MARCHING,
  ) extends GStruct[RayMarchState]

  /** Parameters for rendering */
  case class RenderParams(gridSize: Int32) extends GStruct[RenderParams]

  /** Layout for rendering. */
  case class RenderLayout(
    output: GBuffer[Vec4[Float32]],
    velocity: GBuffer[Vec4[Float32]],
    pressure: GBuffer[Float32],
    density: GBuffer[Float32],
    temperature: GBuffer[Float32],
    obstacles: GBuffer[Float32],
    camera: GUniform[Camera3D],
    params: GUniform[RenderParams],
  ) extends Layout
