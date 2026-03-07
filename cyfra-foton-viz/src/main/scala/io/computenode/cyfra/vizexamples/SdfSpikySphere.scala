package io.computenode.cyfra.vizexamples

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.fotonviz.render.*
import io.computenode.cyfra.fotonviz.render.Vec3Ops.vec3f

/**
 * Glass droplets flowing between two large animated glass cuboids.
 * Mouse interaction adds extra spiky distortion near the cursor ray.
 * 
 * Optimizations applied:
 * - Reduced max steps (80 vs 128)
 * - Larger hit threshold (0.003 vs 0.001)
 * - Tighter max distance (15 vs 30)
 * - Cheaper mouse falloff (no sqrt, linear approx)
 * - Single smooth union for all droplets combined
 * - Simplified displacement when far from mouse
 */
object SdfSpikySphere:

  // ---- Configuration ----
  val WIDTH = 1920
  val HEIGHT = 1280
  val CAM_DIST = 6.0f
  val FOV_DIST = 1.3f

  // ---- Materials ----
  val DROPLET_GLASS = Material.glass(
    color = (0.97f, 0.98f, 1.0f),
    ior = 1.5f,
    dispersion = 0.02f,
    absorption = 0.08f,
  )

  val CUBOID_GLASS = Material.glass(
    color = (0.92f, 0.95f, 1.0f),
    ior = 1.45f,
    dispersion = 0.015f,
    absorption = 0.04f,
  )

  case class Config(
    time: Float32,
    width: Float32,
    height: Float32,
    camDist: Float32,
    fovDist: Float32,
    mouseX: Float32,
    mouseY: Float32,
  ) extends GStruct[Config]

  // ---- Scene Constants ----
  val TOP_CUBOID_Y = 2.5f
  val BOTTOM_CUBOID_Y = -2.5f
  val CUBOID_SIZE_X = 2.5f
  val CUBOID_SIZE_Y = 0.8f
  val CUBOID_SIZE_Z = 2.0f

  val DROPLET_RADIUS = 0.35f
  val NUM_DROPLETS = 5
  val DROPLET_CYCLE = 4.0f

  // Mouse interaction
  val MOUSE_EFFECT_RADIUS = 1.5f
  val MOUSE_EFFECT_RADIUS_SQ = MOUSE_EFFECT_RADIUS * MOUSE_EFFECT_RADIUS
  val MOUSE_SPIKE_INTENSITY = 0.25f
  val MOUSE_SPIKE_FREQ = 15.0f

  // ---- Optimized Distance to Ray (squared, no sqrt) ----

  /** Compute squared distance from point p to ray. Cheaper than actual distance. */
  def distanceToRaySq(p: Vec3[Float32], rayOrigin: Vec3[Float32], rayDir: Vec3[Float32]): Float32 =
    val opX = p.x - rayOrigin.x
    val opY = p.y - rayOrigin.y
    val opZ = p.z - rayOrigin.z
    val t = max(opX * rayDir.x + opY * rayDir.y + opZ * rayDir.z, 0.0f)
    val dx = opX - rayDir.x * t
    val dy = opY - rayDir.y * t
    val dz = opZ - rayDir.z * t
    dx * dx + dy * dy + dz * dz

  /** Mouse-based displacement. Smooth falloff, no branching. */
  def mouseDistortion(p: Vec3[Float32], time: Float32, distSq: Float32): Float32 =
    // Smooth falloff using 1/(1+x^2) style - no sqrt, no branch
    val falloff = MOUSE_EFFECT_RADIUS_SQ / (MOUSE_EFFECT_RADIUS_SQ + distSq * 4.0f)
    val intensity = falloff * falloff * MOUSE_SPIKE_INTENSITY
    val fastTime = time * 8.0f
    val spike = sin(p.x * MOUSE_SPIKE_FREQ + fastTime) * 
                sin(p.y * MOUSE_SPIKE_FREQ - fastTime * 0.7f) * 
                sin(p.z * MOUSE_SPIKE_FREQ + fastTime * 0.5f)
    -spike * intensity

  // ---- Displacement Functions ----

  /** Spiky displacement for droplets. */
  def dropletSpikes(p: Vec3[Float32], time: Float32, intensity: Float32): Float32 =
    val freq = 8.0f
    val spike = sin(p.x * freq) * sin(p.y * freq) * sin(p.z * freq) +
                sin(p.x * 6.0f + time) * sin(p.y * 7.0f) * cos(p.z * 6.0f - time * 0.8f) * 0.5f
    -spike * intensity * 0.7f

  /** Wavy displacement for cuboid surfaces. */
  def cuboidWaves(p: Vec3[Float32], time: Float32, speed: Float32): Float32 =
    val t = time * speed
    sin(p.x * 1.5f + t) * cos(p.z * 2.0f - t * 0.6f) * 0.05f

  // ---- SDF Primitives ----

  /** Animated rounded box SDF. */
  def animatedCuboid(
    p: Vec3[Float32],
    center: Vec3[Float32],
    halfSize: Vec3[Float32],
    time: Float32,
    waveSpeed: Float32,
    mouseDistort: Float32,
  ): Float32 =
    val lx = p.x - center.x
    val ly = p.y - center.y
    val lz = p.z - center.z
    val wave = cuboidWaves(vec3(lx, ly, lz), time, waveSpeed)

    val qx = abs(lx) - halfSize.x
    val qy = abs(ly) - halfSize.y
    val qz = abs(lz) - halfSize.z
    val outside = sqrt(max(qx, 0.0f) * max(qx, 0.0f) + max(qy, 0.0f) * max(qy, 0.0f) + max(qz, 0.0f) * max(qz, 0.0f))
    val inside = min(max(qx, max(qy, qz)), 0.0f)
    outside + inside - 0.1f + wave + mouseDistort

  // ---- Droplet Animation ----

  def dropletState(time: Float32, index: Int32): (Float32, Float32, Float32, Float32) =
    val phase = index.asFloat / NUM_DROPLETS.toFloat
    val t = time / DROPLET_CYCLE + phase
    val cyclePos = t - t.asInt.asFloat

    val submerge = DROPLET_RADIUS * 2.5f
    val bottomY = BOTTOM_CUBOID_Y + CUBOID_SIZE_Y - submerge
    val topY = TOP_CUBOID_Y - CUBOID_SIZE_Y + submerge
    val dropletY = bottomY + (topY - bottomY) * cyclePos

    val fadeZone = 0.12f
    val visibility = when(cyclePos < fadeZone)(cyclePos / fadeZone)
      .elseWhen(cyclePos > 1.0f - fadeZone)((1.0f - cyclePos) / fadeZone)
      .otherwise(1.0f)

    val spikeIntensity = 0.12f + abs(cyclePos - 0.5f) * 0.08f
    val driftX = sin(cyclePos * 3.14159f * 2.0f + phase * 6.28f) * 0.3f

    (dropletY, visibility, spikeIntensity, driftX)

  /** Single droplet SDF. */
  def dropletSdf(
    p: Vec3[Float32],
    center: Vec3[Float32],
    radius: Float32,
    time: Float32,
    timeOffset: Float32,
    spikeIntensity: Float32,
    visibility: Float32,
    mouseDistort: Float32,
  ): Float32 =
    val lx = p.x - center.x
    val ly = p.y - center.y
    val lz = p.z - center.z
    val dist = sqrt(lx * lx + ly * ly + lz * lz) - radius
    val spikes = dropletSpikes(vec3(lx, ly, lz), time + timeOffset, spikeIntensity)
    dist + spikes + mouseDistort + (1.0f - visibility) * 10.0f

  // ---- Scene SDF ----
  val BLEND_K = 0.25f  // Slightly smaller for speed

  def sceneSdf(
    p: Vec3[Float32],
    time: Float32,
    mouseRayOrigin: Vec3[Float32],
    mouseRayDir: Vec3[Float32],
  ): SdfResult =
    // Compute mouse distortion once
    val distSq = distanceToRaySq(p, mouseRayOrigin, mouseRayDir)
    val mouseDistort = mouseDistortion(p, time, distSq)

    // Top cuboid
    val topDist = animatedCuboid(p, vec3(0.0f, TOP_CUBOID_Y, 0.0f), 
      vec3(CUBOID_SIZE_X, CUBOID_SIZE_Y, CUBOID_SIZE_Z), time, 0.25f, mouseDistort)
    
    // Bottom cuboid
    val bottomDist = animatedCuboid(p, vec3(0.0f, BOTTOM_CUBOID_Y, 0.0f),
      vec3(CUBOID_SIZE_X, CUBOID_SIZE_Y, CUBOID_SIZE_Z), time * 0.8f, 0.3f, mouseDistort)

    // Combine cuboids first
    val cuboidsDist = min(topDist, bottomDist)
    val cuboids = SdfPrimitives.withMaterial(cuboidsDist, materialId = 1)

    // All droplets - use min() to combine, then single smooth union with cuboids
    val d0 = dropletState(time, 0)
    val drop0 = dropletSdf(p, vec3(d0._4, d0._1, 0.0f), DROPLET_RADIUS, time, 0f, d0._3, d0._2, mouseDistort)

    val d1 = dropletState(time, 1)
    val drop1 = dropletSdf(p, vec3(-0.6f + d1._4 * 0.5f, d1._1, 0.5f), DROPLET_RADIUS * 0.85f, time, 1f, d1._3, d1._2, mouseDistort)

    val d2 = dropletState(time, 2)
    val drop2 = dropletSdf(p, vec3(0.7f + d2._4 * 0.3f, d2._1, -0.4f), DROPLET_RADIUS * 0.9f, time, 2f, d2._3, d2._2, mouseDistort)

    val d3 = dropletState(time, 3)
    val drop3 = dropletSdf(p, vec3(0.4f + d3._4 * 0.4f, d3._1, 0.6f), DROPLET_RADIUS * 0.75f, time, 3f, d3._3, d3._2, mouseDistort)

    val d4 = dropletState(time, 4)
    val drop4 = dropletSdf(p, vec3(-0.5f + d4._4 * 0.6f, d4._1, -0.5f), DROPLET_RADIUS * 0.8f, time, 4f, d4._3, d4._2, mouseDistort)

    // Combine all droplets with simple min (they don't overlap)
    val allDropletsDist = min(drop0, min(drop1, min(drop2, min(drop3, drop4))))
    val droplets = SdfPrimitives.withMaterial(allDropletsDist, materialId = 0)

    // Single smooth union between cuboids and droplets
    SdfOps.smoothUnion(cuboids, droplets, BLEND_K)

  // ---- Render Program ----
  def renderProgram(runner: SdfRunner[Config]): GProgram[Int, runner.SdfLayout] =
    GProgram.static[Int, runner.SdfLayout](
      layout = n => runner.SdfLayout(GBuffer[Vec4[Float16]](n), GUniform[Config]()),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read

      val materialPack = MaterialPack(
        mat0 = DROPLET_GLASS.toProps,
        mat1 = CUBOID_GLASS.toProps,
        mat2 = Material.Empty.toProps,
        mat3 = Material.Empty.toProps,
      )

      GIO.when(invocId < cfg.width.asInt * cfg.height.asInt):
        val px = invocId.mod(cfg.width.asInt)
        val py = invocId / cfg.width.asInt
        val u = (px.asFloat / cfg.width) * 2.0f - 1.0f
        val v = 1.0f - (py.asFloat / cfg.height) * 2.0f

        val camPos = vec3(0.0f, 0.0f, -cfg.camDist)
        val rayDir = normalize(vec3f(u * (cfg.width / cfg.height), v, cfg.fovDist))
        val lightDir = normalize(vec3f(0.5f, 0.4f, -0.7f))
        val mouseRayDir = normalize(vec3f(cfg.mouseX * (cfg.width / cfg.height), cfg.mouseY, cfg.fovDist))

        def sdf(p: Vec3[Float32]): SdfResult = sceneSdf(p, cfg.time, camPos, mouseRayDir)

        val result = Raymarch.march(
          origin = camPos,
          dir = rayDir,
          sdf = sdf,
          materials = materialPack,
          lightDir = lightDir,
          time = cfg.time,
          maxSteps = 80,        // Reduced from 128
          maxDist = 15.0f,      // Reduced from 30
          hitThreshold = 0.003f, // Increased from 0.001
        )

        val vignetteFactor = 1.0f - (u * u + v * v) * 0.1f
        val finalColor = vec3(
          clamp(result.color.x * vignetteFactor, 0.0f, 1.0f),
          clamp(result.color.y * vignetteFactor, 0.0f, 1.0f),
          clamp(result.color.z * vignetteFactor, 0.0f, 1.0f),
        )

        GIO.write(layout.pixels, invocId, Pixels.rgba16f(finalColor))

  // ---- Main ----
  @main def runSdfSpikySphere(): Unit =
    val runner = SdfRunner[Config](WIDTH, HEIGHT, "Glass Droplets - Interactive")

    runner.run(
      program = renderProgram(runner),
      makeConfig = ctx =>
        Config(
          time = ctx.time,
          width = ctx.width.toFloat,
          height = ctx.height.toFloat,
          camDist = CAM_DIST,
          fovDist = FOV_DIST,
          mouseX = ctx.mouseX,
          mouseY = ctx.mouseY,
        ),
    )
