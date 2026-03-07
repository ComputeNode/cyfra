package io.computenode.cyfra.vizexamples

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.fotonviz.render.*
import io.computenode.cyfra.fotonviz.render.Vec3Ops.vec3f

/**
 * Dark scene with shallow water reflecting 5 glowing white poles.
 * Mouse interaction creates ripples on the water surface.
 */
object WaterPoles:

  val WIDTH = 1920
  val HEIGHT = 1080
  val CAM_DIST = 10.0f
  val FOV_DIST = 1.5f

  // Scene layout
  val WATER_SURFACE_Y = 0.0f
  val FLOOR_Y = -0.5f // Floor below water (only for pole bases)

  // Pole configuration
  val POLE_RADIUS = 0.15f
  val POLE_HEIGHT = 3.0f
  val POLE_Y_BASE = FLOOR_Y
  val NUM_POLES = 5

  // Pole positions (arranged in a loose arc)
  val polePositions: Array[(Float, Float)] = Array(
    (-3.0f, 2.0f),
    (-1.5f, 0.8f),
    (0.0f, 0.0f),
    (1.5f, 0.8f),
    (3.0f, 2.0f),
  )

  // Mouse ripple parameters
  val RIPPLE_RADIUS = 3.0f
  val RIPPLE_SPEED = 5.0f
  val RIPPLE_AMPLITUDE = 0.12f
  val RIPPLE_FREQUENCY = 8.0f

  // Materials
  val WATER_MAT = Material.Def(
    isTransparent = true,
    ior = 1.33f,
    dispersion = 0.01f,
    absorption = 0.1f,
    color = (0.7f, 0.8f, 0.9f),
    roughness = 0.0f,
  )

  val POLE_MAT = Material.Def(
    isTransparent = false,
    color = (1.0f, 1.0f, 1.0f),
    roughness = 0.05f,
    metallic = 0.0f,
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

  /** Compute intersection of ray with water plane (for mouse hover position). */
  def rayPlaneIntersect(
    rayOrigin: Vec3[Float32],
    rayDir: Vec3[Float32],
    planeY: Float32,
  ): Vec3[Float32] =
    val t = (planeY - rayOrigin.y) / rayDir.y
    vec3(
      rayOrigin.x + rayDir.x * t,
      planeY,
      rayOrigin.z + rayDir.z * t,
    )

  /** Ripple displacement based on distance from mouse point. */
  def rippleDisplacement(
    p: Vec3[Float32],
    mouseWorldPos: Vec3[Float32],
    time: Float32,
  ): Float32 =
    val dx = p.x - mouseWorldPos.x
    val dz = p.z - mouseWorldPos.z
    val dist = sqrt(dx * dx + dz * dz)

    // Concentric ripples centered on mouse - continuous, no decay
    val ripplePhase = dist * RIPPLE_FREQUENCY - time * RIPPLE_SPEED
    val rippleWave = sin(ripplePhase)

    // Smooth falloff from mouse position
    val falloff = exp(-dist * dist / (RIPPLE_RADIUS * RIPPLE_RADIUS))

    rippleWave * RIPPLE_AMPLITUDE * falloff

  /** Ambient water surface waves (subtle). */
  def ambientWaves(p: Vec3[Float32], time: Float32): Float32 =
    val w1 = sin(p.x * 1.5f + time * 0.8f) * cos(p.z * 1.2f + time * 0.6f) * 0.02f
    val w2 = sin(p.x * 2.5f - time * 0.5f) * sin(p.z * 2.0f + time * 0.4f) * 0.01f
    w1 + w2

  /** Water surface SDF - infinite plane with wavy displacement. */
  def waterSurface(p: Vec3[Float32], mouseWorldPos: Vec3[Float32], time: Float32): Float32 =
    val ripple = rippleDisplacement(p, mouseWorldPos, time)
    val ambient = ambientWaves(p, time)
    val displacement = ripple + ambient

    // Simple infinite plane at WATER_SURFACE_Y with displacement
    p.y - WATER_SURFACE_Y - displacement

  /** Single pole SDF. */
  def poleSdf(p: Vec3[Float32], centerX: Float32, centerZ: Float32): Float32 =
    val dx = p.x - centerX
    val dz = p.z - centerZ
    val radialDist = sqrt(dx * dx + dz * dz) - POLE_RADIUS
    val halfHeight = POLE_HEIGHT / 2.0f
    val centerY = POLE_Y_BASE + halfHeight
    val heightDist = abs(p.y - centerY) - halfHeight
    min(max(radialDist, heightDist), 0.0f) +
      sqrt(max(radialDist, 0.0f) * max(radialDist, 0.0f) + max(heightDist, 0.0f) * max(heightDist, 0.0f))

  /** Combined poles SDF. */
  def allPolesSdf(p: Vec3[Float32]): Float32 =
    val d0 = poleSdf(p, polePositions(0)._1, polePositions(0)._2)
    val d1 = poleSdf(p, polePositions(1)._1, polePositions(1)._2)
    val d2 = poleSdf(p, polePositions(2)._1, polePositions(2)._2)
    val d3 = poleSdf(p, polePositions(3)._1, polePositions(3)._2)
    val d4 = poleSdf(p, polePositions(4)._1, polePositions(4)._2)
    min(d0, min(d1, min(d2, min(d3, d4))))

  /** Compute glow intensity from all poles at a given point. Pure white, no pulsing. */
  def poleGlow(p: Vec3[Float32]): Vec3[Float32] =
    def singlePoleGlow(px: Float32, pz: Float32): Float32 =
      val dx = p.x - px
      // Glow emanates from entire pole (not just top)
      val poleTopY = POLE_Y_BASE + POLE_HEIGHT
      val poleMidY = POLE_Y_BASE + POLE_HEIGHT / 2.0f
      val dy = p.y - poleMidY
      val dz = p.z - pz
      val distSq = dx * dx + dy * dy + dz * dz
      // Inverse square falloff for point light
      val intensity = 6.0f / (1.0f + distSq * 0.25f)
      intensity

    val g0 = singlePoleGlow(polePositions(0)._1, polePositions(0)._2)
    val g1 = singlePoleGlow(polePositions(1)._1, polePositions(1)._2)
    val g2 = singlePoleGlow(polePositions(2)._1, polePositions(2)._2)
    val g3 = singlePoleGlow(polePositions(3)._1, polePositions(3)._2)
    val g4 = singlePoleGlow(polePositions(4)._1, polePositions(4)._2)
    val totalGlow = g0 + g1 + g2 + g3 + g4
    // Pure white glow - no color tint
    vec3(totalGlow, totalGlow, totalGlow)

  /** Scene SDF combining water and poles (no floor - water goes to horizon). */
  def sceneSdf(p: Vec3[Float32], mouseWorldPos: Vec3[Float32], time: Float32): SdfResult =
    // Infinite water plane
    val waterDist = waterSurface(p, mouseWorldPos, time)
    val water = SdfResult(waterDist, materialId = 0)

    // Poles (white emissive)
    val polesDist = allPolesSdf(p)
    val poles = SdfResult(polesDist, materialId = 2)

    // Poles take priority over water
    SdfOps.union(poles, water)

  /** Custom environment for black background with subtle gradient. */
  def darkEnvironment(dir: Vec3[Float32]): Vec3[Float32] =
    // Very dark with subtle blue-ish tint at horizon
    val horizonFactor = exp(-abs(dir.y) * 2.0f)
    val base = 0.01f
    vec3(
      base + horizonFactor * 0.02f,
      base + horizonFactor * 0.025f,
      base + horizonFactor * 0.04f,
    )

  /** Trace a reflection ray to find if it hits a pole. Only traces against poles, ignores floor/water. */
  def traceReflection(
    origin: Vec3[Float32],
    dir: Vec3[Float32],
  ): Vec3[Float32] =
    import io.computenode.cyfra.dsl.collections.GSeq

    // Ray march against poles only - not full scene
    val maxReflectSteps = 50
    val maxReflectDist = 25.0f
    val hitThreshold = 0.01f

    case class ReflectState(
      px: Float32, py: Float32, pz: Float32,
      dist: Float32,
      hitPole: Float32,
      done: Float32,
    ) extends GStruct[ReflectState]

    val initState = ReflectState(origin.x, origin.y, origin.z, 0.0f, 0.0f, 0.0f)

    val result = GSeq.gen[Int32](0, _ + 1).limit(maxReflectSteps)
      .fold(initState, (state: ReflectState, _: Int32) =>
        when(state.done > 0.5f)(state).otherwise:
          when(state.dist > maxReflectDist):
            ReflectState(state.px, state.py, state.pz, state.dist, 0.0f, 1.0f)
          .otherwise:
            val pos = vec3(state.px, state.py, state.pz)
            // Only trace against poles - not full scene SDF
            val poleDist = allPolesSdf(pos)
            val d = abs(poleDist)

            val isHit = d < hitThreshold
            val hitPole = when(isHit)(1.0f).otherwise(0.0f)

            val stepSize = max(d * 0.9f, hitThreshold)
            val newPos = pos + dir * stepSize

            val isDone = when(isHit)(1.0f).otherwise(0.0f)
            ReflectState(newPos.x, newPos.y, newPos.z, state.dist + stepSize, hitPole, isDone)
      )

    // If we hit a pole, return bright white. Otherwise dark.
    when(result.hitPole > 0.5f):
      vec3(1.0f, 1.0f, 1.0f) // Bright white for pole reflection
    .otherwise:
      vec3(0.0f, 0.0f, 0.0f)

  /** Modified raymarch that uses dark environment and adds pole glow with pixel-footprint AA. */
  def marchWithGlow(
    origin: Vec3[Float32],
    dir: Vec3[Float32],
    sdf: Vec3[Float32] => SdfResult,
    materials: MaterialPack,
    time: Float32,
    maxSteps: Int,
    maxDist: Float32,
    hitThreshold: Float32,
    pixelScale: Float32,
  ): Raymarch.MarchResult =
    import Raymarch.*

    val initState = RayState(
      posX = origin.x, posY = origin.y, posZ = origin.z,
      dirX = dir.x, dirY = dir.y, dirZ = dir.z,
      colorR = 0.0f, colorG = 0.0f, colorB = 0.0f,
      throughputR = 1.0f, throughputG = 1.0f, throughputB = 1.0f,
      totalDist = 0.0f,
      insideGlass = 0.0f,
      glassPathLen = 0.0f,
      done = 0.0f,
    )

    import io.computenode.cyfra.dsl.collections.GSeq

    val result = GSeq.gen[Int32](0, _ + 1).limit(maxSteps)
      .fold(initState, (state: RayState, _: Int32) =>
        val pos = vec3(state.posX, state.posY, state.posZ)
        val rayD = vec3(state.dirX, state.dirY, state.dirZ)
        val color = vec3(state.colorR, state.colorG, state.colorB)
        val throughput = vec3(state.throughputR, state.throughputG, state.throughputB)
        val totalDist = state.totalDist
        val insideGlass = state.insideGlass
        val glassPath = state.glassPathLen

        when(state.done > 0.5f)(state).otherwise:
          when(totalDist > maxDist):
            val env = darkEnvironment(rayD)
            val finalColor = vec3(
              color.x + throughput.x * env.x,
              color.y + throughput.y * env.y,
              color.z + throughput.z * env.z,
            )
            RayState(
              posX = pos.x, posY = pos.y, posZ = pos.z,
              dirX = rayD.x, dirY = rayD.y, dirZ = rayD.z,
              colorR = finalColor.x, colorG = finalColor.y, colorB = finalColor.z,
              throughputR = 0.0f, throughputG = 0.0f, throughputB = 0.0f,
              totalDist = totalDist,
              insideGlass = 0.0f,
              glassPathLen = 0.0f,
              done = 1.0f,
            )
          .otherwise:
            val evalResult = sdf(pos)
            val signedDist = evalResult.distance
            val d = abs(signedDist)
            val mats = evalResult.materials

            val matColor = getMaterialColor(materials, mats)
            val isTransparent = getMaterialProperty(materials, mats, _.isTransparent)
            val ior = getMaterialProperty(materials, mats, _.ior)
            val roughness = getMaterialProperty(materials, mats, _.roughness)

            // Pixel-footprint based anti-aliasing using smoothstep
            val pixelWidth = max(totalDist * pixelScale, 0.001f)
            val edgeSoftness = pixelWidth * 3.0f  // Wider soft zone
            
            // Solid threshold extends to edgeSoftness for AA soft zone
            val solidThreshold = max(edgeSoftness, hitThreshold)
            
            val isGlass = isTransparent > 0.5f
            val isGlassHit = (d < hitThreshold) && isGlass
            val isSolidHit = (d < solidThreshold) && !isGlass
            val isHit = isGlassHit || isSolidHit
            
            // Smoothstep coverage: smooth S-curve from 1.0 at d=0 to 0.0 at d=edgeSoftness
            val t = clamp(d / edgeSoftness, 0.0f, 1.0f)
            val smoothT = t * t * (3.0f - 2.0f * t)  // smoothstep polynomial
            val solidCoverage = when(isSolidHit):
              1.0f - smoothT
            .otherwise(0.0f)

            val regularN = when(isHit)(normal(pos, p => sdf(p).distance)).otherwise(vec3(0.0f, 1.0f, 0.0f))
            val glassN = when(isHit && isGlass)(smoothNormal(pos, p => sdf(p).distance)).otherwise(regularN)
            val n = when(isGlass)(glassN).otherwise(regularN)
            val orientedN = when(insideGlass > 0.5f)(-n).otherwise(n)

            val viewDir = vec3(-rayD.x, -rayD.y, -rayD.z)
            val cosTheta = max(viewDir dot orientedN, 0.0f)

            // Glass handling (for water)
            val entering = insideGlass < 0.5f
            val eta = when(entering)(1.0f / ior).otherwise(ior)
            val fresnelFactor = fresnel(cosTheta, ior)

            val reflectDir = reflect(rayD, orientedN)
            val reflectedEnv = darkEnvironment(reflectDir)

            // Trace reflection ray against poles only
            val reflectOrigin = pos + orientedN * 0.05f // Offset to avoid self-intersection
            val reflectedColor = traceReflection(reflectOrigin, reflectDir)

            val exitAbsorption = when(entering)(vec3(1.0f, 1.0f, 1.0f)).otherwise(beerAbsorption(glassPath, materials.mat0))

            val glassReflectionColor = when(isGlassHit):
              val reflectionIntensity = fresnelFactor * 0.95f
              // Use actual traced reflection
              vec3(
                color.x + throughput.x * (reflectedEnv.x + reflectedColor.x) * reflectionIntensity * exitAbsorption.x,
                color.y + throughput.y * (reflectedEnv.y + reflectedColor.y) * reflectionIntensity * exitAbsorption.y,
                color.z + throughput.z * (reflectedEnv.z + reflectedColor.z) * reflectionIntensity * exitAbsorption.z,
              )
            .otherwise(color)

            val transmission = 1.0f - fresnelFactor * 0.8f
            val glassThroughput = when(isGlassHit):
              vec3(
                throughput.x * transmission * exitAbsorption.x,
                throughput.y * transmission * exitAbsorption.y,
                throughput.z * transmission * exitAbsorption.z,
              )
            .otherwise(throughput)

            val refractResult = refract(rayD, orientedN, eta)
            val refractDir = normalize(vec3(refractResult.x, refractResult.y, refractResult.z))
            val tir = refractResult.w > 0.5f

            val glassNewDir = when(isGlassHit):
              when(tir)(reflectDir).otherwise(refractDir)
            .otherwise(rayD)

            val newInsideGlass = when(isGlassHit):
              when(tir)(insideGlass).otherwise(when(entering)(1.0f).otherwise(0.0f))
            .otherwise(insideGlass)

            val newGlassPath = when(isGlassHit):
              when(entering)(0.0f).otherwise(0.0f)
            .otherwise:
              when(insideGlass > 0.5f)(glassPath + d).otherwise(0.0f)

            // Solid surface hit with AA - ACCUMULATE color contribution weighted by coverage and throughput
            val poleEmissive = vec3(1.0f, 1.0f, 1.0f)
            
            // Accumulate solid color contribution (coverage * emissive * throughput)
            val solidContribution = when(isSolidHit):
              vec3(
                solidCoverage * poleEmissive.x * glassThroughput.x,
                solidCoverage * poleEmissive.y * glassThroughput.y,
                solidCoverage * poleEmissive.z * glassThroughput.z,
              )
            .otherwise(vec3(0.0f, 0.0f, 0.0f))
            
            // Add solid contribution to accumulated color
            val finalColor = when(isSolidHit):
              vec3(
                glassReflectionColor.x + solidContribution.x,
                glassReflectionColor.y + solidContribution.y,
                glassReflectionColor.z + solidContribution.z,
              )
            .otherwise(glassReflectionColor)
            
            // Reduce throughput based on coverage (partial occlusion)
            val finalThroughput = when(isSolidHit):
              vec3(
                glassThroughput.x * (1.0f - solidCoverage),
                glassThroughput.y * (1.0f - solidCoverage),
                glassThroughput.z * (1.0f - solidCoverage),
              )
            .otherwise(glassThroughput)

            val stepSize = max(d * 0.8f, hitThreshold)
            val glassOffset = 0.08f
            val offset = when(isGlassHit)(glassOffset).otherwise(stepSize)
            val newDir = when(isGlassHit)(glassNewDir).otherwise(rayD)
            val newPos = pos + newDir * offset
            val newDist = totalDist + offset

            // Done when solid surface is hit with good coverage
            val isDone = when(isSolidHit && (solidCoverage > 0.9f))(1.0f).otherwise(0.0f)
            
            RayState(
              posX = newPos.x, posY = newPos.y, posZ = newPos.z,
              dirX = newDir.x, dirY = newDir.y, dirZ = newDir.z,
              colorR = finalColor.x, colorG = finalColor.y, colorB = finalColor.z,
              throughputR = finalThroughput.x, throughputG = finalThroughput.y, throughputB = finalThroughput.z,
              totalDist = newDist,
              insideGlass = newInsideGlass,
              glassPathLen = newGlassPath,
              done = isDone,
            )
      )

    // Final color
    val finalColor = when(result.done < 0.5f):
      val env = darkEnvironment(vec3(result.dirX, result.dirY, result.dirZ))
      vec3(
        result.colorR + result.throughputR * env.x,
        result.colorG + result.throughputG * env.y,
        result.colorB + result.throughputB * env.z,
      )
    .otherwise(vec3(result.colorR, result.colorG, result.colorB))

    Raymarch.MarchResult(vec3(
      clamp(finalColor.x, 0.0f, 1.0f),
      clamp(finalColor.y, 0.0f, 1.0f),
      clamp(finalColor.z, 0.0f, 1.0f),
    ))

  def renderProgram(runner: SdfRunner[Config]): GProgram[Int, runner.SdfLayout] =
    GProgram.static[Int, runner.SdfLayout](
      layout = n => runner.SdfLayout(GBuffer[Vec4[Float16]](n), GUniform[Config]()),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read

      val materialPack = MaterialPack(
        mat0 = WATER_MAT.toProps,
        mat1 = Material.Empty.toProps, // No floor
        mat2 = POLE_MAT.toProps,
        mat3 = Material.Empty.toProps,
      )

      GIO.when(invocId < cfg.width.asInt * cfg.height.asInt):
        val px = invocId.mod(cfg.width.asInt)
        val py = invocId / cfg.width.asInt
        val u = (px.asFloat / cfg.width) * 2.0f - 1.0f
        val v = 1.0f - (py.asFloat / cfg.height) * 2.0f

        // Camera setup - elevated, looking down at water
        val camAngle = 0.35f // Pitch angle (looking down)
        val camPos = vec3(
          0.0f,
          3.5f,
          -cfg.camDist,
        )

        // Camera rotation (pitch down to see water)
        val cosA = cos(camAngle)
        val sinA = sin(camAngle)

        val aspect = cfg.width / cfg.height
        val rawDir = normalize(vec3f(u * aspect, v, cfg.fovDist))

        // Rotate ray direction (pitch down around X axis)
        val rayDir = normalize(vec3(
          rawDir.x,
          rawDir.y * cosA - rawDir.z * sinA,
          rawDir.y * sinA + rawDir.z * cosA,
        ))

        // Calculate mouse world position on water plane
        val mouseRawDir = normalize(vec3f(cfg.mouseX * aspect, cfg.mouseY, cfg.fovDist))
        val mouseRayDir = normalize(vec3(
          mouseRawDir.x,
          mouseRawDir.y * cosA - mouseRawDir.z * sinA,
          mouseRawDir.y * sinA + mouseRawDir.z * cosA,
        ))
        val mouseWorldPos = rayPlaneIntersect(camPos, mouseRayDir, WATER_SURFACE_Y)

        def sdf(p: Vec3[Float32]): SdfResult = sceneSdf(p, mouseWorldPos, cfg.time)

        // Calculate pixel scale for AA
        val pixelScale = 2.0f / (cfg.fovDist * cfg.height)
        
        val result = marchWithGlow(
          origin = camPos,
          dir = rayDir,
          sdf = sdf,
          materials = materialPack,
          time = cfg.time,
          maxSteps = 100,
          maxDist = 50.0f,
          hitThreshold = 0.005f,
          pixelScale = pixelScale,
        )

        // Bloom effect: bright glow around poles based on proximity
        // Sample multiple points along ray and in nearby directions
        val bloomSample1 = poleGlow(camPos + rayDir * 5.0f)
        val bloomSample2 = poleGlow(camPos + rayDir * 10.0f)
        val bloomSample3 = poleGlow(camPos + rayDir * 15.0f)
        
        // Also sample with slight offset for wider bloom
        val offsetAmount = 0.3f
        val rightOffset = normalize(vec3(rayDir.z, 0.0f, -rayDir.x)) * offsetAmount
        val upOffset = vec3(0.0f, offsetAmount, 0.0f)
        
        val bloomSample4 = poleGlow(camPos + rayDir * 8.0f + rightOffset)
        val bloomSample5 = poleGlow(camPos + rayDir * 8.0f - rightOffset)
        val bloomSample6 = poleGlow(camPos + rayDir * 8.0f + upOffset)
        val bloomSample7 = poleGlow(camPos + rayDir * 8.0f - upOffset)
        
        // Combine bloom samples with distance-based weighting
        val bloomGlow = (bloomSample1 * 0.3f + bloomSample2 * 0.25f + bloomSample3 * 0.15f +
                        bloomSample4 * 0.1f + bloomSample5 * 0.1f + 
                        bloomSample6 * 0.05f + bloomSample7 * 0.05f) * 0.025f

        val finalColor = vec3(
          clamp(result.color.x + bloomGlow.x, 0.0f, 1.0f),
          clamp(result.color.y + bloomGlow.y, 0.0f, 1.0f),
          clamp(result.color.z + bloomGlow.z, 0.0f, 1.0f),
        )

        GIO.write(layout.pixels, invocId, Pixels.rgba16f(finalColor))

  @main def runWaterPoles(): Unit =
    val runner = SdfRunner[Config](WIDTH, HEIGHT, "Water & Glowing Poles")

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
      postProcess = Seq(
        PostProcessEffects.filmLook[Config](
          runner = runner,
          grainStrength = 0.035f,
          vignetteStrength = 0.3f,
          contrast = 1.05f,
          saturation = 0.9f,
        )(
          getWidth = _.width.asInt,
          getHeight = _.height.asInt,
          getTime = _.time,
        ),
      ),
    )
