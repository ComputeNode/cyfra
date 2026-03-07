package io.computenode.cyfra.vizexamples

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.fotonviz.render.*
import io.computenode.cyfra.fotonviz.render.Vec3Ops.vec3f

/**
 * Music visualizer with CPU-computed FFT (passed as uniforms).
 * Water surface responds to audio frequencies - bass creates waves, treble adds shimmer.
 * Poles pulse and glow with the beat.
 */
object MusicVisualizer:

  val WIDTH = 1920
  val HEIGHT = 1080
  val CAM_DIST = 10.0f
  val FOV_DIST = 1.5f

  // Scene layout
  val WATER_SURFACE_Y = 0.0f

  // Pole configuration
  val POLE_RADIUS = 0.12f
  val POLE_HEIGHT = 3.0f
  val POLE_Y_BASE = -0.5f
  val NUM_POLES = 5

  // Pole positions (arranged in a loose arc)
  val polePositions: Array[(Float, Float)] = Array(
    (-3.0f, 2.0f),
    (-1.5f, 0.8f),
    (0.0f, 0.0f),
    (1.5f, 0.8f),
    (3.0f, 2.0f),
  )

  // Materials
  val WATER_MAT = Material.Def(
    isTransparent = true,
    ior = 1.33f,
    dispersion = 0.01f,
    absorption = 0.05f,
    color = (0.6f, 0.7f, 0.9f),
    roughness = 0.0f,
  )

  val POLE_MAT = Material.Def(
    isTransparent = false,
    color = (1.0f, 1.0f, 1.0f),
    roughness = 0.05f,
    metallic = 0.0f,
  )

  /**
   * GPU-side config with pre-computed frequency band levels (from CPU FFT).
   */
  case class Config(
    time: Float32,
    width: Float32,
    height: Float32,
    camDist: Float32,
    fovDist: Float32,
    bass: Float32,
    lowMid: Float32,
    highMid: Float32,
    treble: Float32,
  ) extends GStruct[Config]

  /**
   * Water spikes with audio-driven amplitude and vibration.
   * 
   * Key insight: Bass level controls HEIGHT, not oscillation period.
   * When bass is strong: spikes rise AND vibrate rapidly on top.
   * Base displacement is stable, high-freq vibration adds on top.
   */
  def audioWaterDisplacement(
    p: Vec3[Float32],
    time: Float32,
    bass: Float32,
    lowMid: Float32,
    highMid: Float32,
    treble: Float32,
  ): Float32 =
    val d = length(vec2(p.x, p.z)) / 10f
    val intensity = max(0f, -(d * d) + 1f)


    val freq1 = 4.5f
    val amp1 = 0.2f
    // BASS
    val spike1 = exp(sin(p.x * freq1) + sin(p.z * freq1)) * bass * bass * amp1
    
    val freq2 = 31f
    val modFreq = 30f
    val amp2 = 0.05f * (sin(time * modFreq) * 0.5f + 0.5f)
    // BASS
    val spike2 = (sin(p.x * freq2) + sin(p.z * freq2)) * spike1 * amp2

    
    val bassSpikes = spike1 + spike2 


    bassSpikes * intensity// + midSpikes + hiMidSpikes + trebleSpikes

  /** Water surface SDF with audio-reactive displacement. */
  def waterSurface(
    p: Vec3[Float32],
    time: Float32,
    bass: Float32,
    lowMid: Float32,
    highMid: Float32,
    treble: Float32,
  ): Float32 =
    val displacement = audioWaterDisplacement(p, time, bass, lowMid, highMid, treble)
    p.y - WATER_SURFACE_Y - displacement

  /** Single pole SDF - fixed height, no audio modulation. */
  def poleSdf(p: Vec3[Float32], centerX: Float32, centerZ: Float32): Float32 =
    val dx = p.x - centerX
    val dz = p.z - centerZ
    val radialDist = sqrt(dx * dx + dz * dz) - POLE_RADIUS
    val halfHeight = POLE_HEIGHT / 2.0f
    val centerY = POLE_Y_BASE + halfHeight
    val heightDist = abs(p.y - centerY) - halfHeight
    min(max(radialDist, heightDist), 0.0f) +
      sqrt(max(radialDist, 0.0f) * max(radialDist, 0.0f) + max(heightDist, 0.0f) * max(heightDist, 0.0f))

  /** Combined poles SDF - static poles. */
  def allPolesSdf(p: Vec3[Float32]): Float32 =
    val d0 = poleSdf(p, polePositions(0)._1, polePositions(0)._2)
    val d1 = poleSdf(p, polePositions(1)._1, polePositions(1)._2)
    val d2 = poleSdf(p, polePositions(2)._1, polePositions(2)._2)
    val d3 = poleSdf(p, polePositions(3)._1, polePositions(3)._2)
    val d4 = poleSdf(p, polePositions(4)._1, polePositions(4)._2)
    min(d0, min(d1, min(d2, min(d3, d4))))

  /** Compute glow intensity from all poles - constant white glow. */
  def poleGlow(p: Vec3[Float32]): Vec3[Float32] =
    def singlePoleGlow(px: Float32, pz: Float32): Float32 =
      val dx = p.x - px
      val poleMidY = POLE_Y_BASE + POLE_HEIGHT / 2.0f
      val dy = p.y - poleMidY
      val dz = p.z - pz
      val distSq = dx * dx + dy * dy + dz * dz
      6.0f / (1.0f + distSq * 0.25f)

    val g0 = singlePoleGlow(polePositions(0)._1, polePositions(0)._2)
    val g1 = singlePoleGlow(polePositions(1)._1, polePositions(1)._2)
    val g2 = singlePoleGlow(polePositions(2)._1, polePositions(2)._2)
    val g3 = singlePoleGlow(polePositions(3)._1, polePositions(3)._2)
    val g4 = singlePoleGlow(polePositions(4)._1, polePositions(4)._2)
    val totalGlow = g0 + g1 + g2 + g3 + g4
    vec3(totalGlow, totalGlow, totalGlow)


  /** Dark environment with subtle audio-reactive tint. */
  def darkEnvironment(dir: Vec3[Float32], bass: Float32): Vec3[Float32] =
    val horizonFactor = exp(-abs(dir.y) * 2.0f)
    val base = 0.01f + bass * 0.02f // Subtle pulse with bass
    vec3(
      base + horizonFactor * 0.02f,
      base + horizonFactor * 0.025f,
      base + horizonFactor * 0.04f,
    )

  /** Trace reflection ray against poles only. */
  def traceReflection(
    origin: Vec3[Float32],
    dir: Vec3[Float32],
  ): Vec3[Float32] =
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
            val poleDist = allPolesSdf(pos)
            val d = abs(poleDist)

            val isHit = d < hitThreshold
            val hitPole = when(isHit)(1.0f).otherwise(0.0f)

            val stepSize = max(d * 0.9f, hitThreshold)
            val newPos = pos + dir * stepSize

            val isDone = when(isHit)(1.0f).otherwise(0.0f)
            ReflectState(newPos.x, newPos.y, newPos.z, state.dist + stepSize, hitPole, isDone)
      )

    when(result.hitPole > 0.5f):
      vec3(1.0f, 1.0f, 1.0f)
    .otherwise:
      vec3(0.0f, 0.0f, 0.0f)

  // ============================================================================
  // CLEAN RAYTRACER - From scratch, proper design
  // ============================================================================
  
  /** Ray state for clean raytracer. */
  case class CleanRayState(
    px: Float32, py: Float32, pz: Float32,           // Position
    dx: Float32, dy: Float32, dz: Float32,           // Direction
    cr: Float32, cg: Float32, cb: Float32,           // Accumulated color
    tr: Float32, tg: Float32, tb: Float32,           // Throughput (energy remaining)
    dist: Float32,                                    // Total distance traveled
    done: Float32,                                    // 1.0 if ray terminated
  ) extends GStruct[CleanRayState]
  given GStructSchema[CleanRayState] = GStructSchema.derived

  /** Clean raymarch - proper water and pole rendering.
    * 
    * Design:
    * - Water and poles have SEPARATE SDFs (no union ambiguity)
    * - Water: dark reflective surface, partially transparent
    * - Poles: emissive white, fully opaque
    */
  def marchWithGlow(
    origin: Vec3[Float32],
    dir: Vec3[Float32],
    time: Float32,
    bass: Float32,
    lowMid: Float32,
    highMid: Float32,
    treble: Float32,
    maxSteps: Int,
    maxDist: Float32,
    hitThreshold: Float32,
    pixelScale: Float32,
  ): Raymarch.MarchResult =

    val initState = CleanRayState(
      px = origin.x, py = origin.y, pz = origin.z,
      dx = dir.x, dy = dir.y, dz = dir.z,
      cr = 0.0f, cg = 0.0f, cb = 0.0f,
      tr = 1.0f, tg = 1.0f, tb = 1.0f,
      dist = 0.0f,
      done = 0.0f,
    )

    val result = GSeq.gen[Int32](0, _ + 1).limit(maxSteps)
      .fold(initState, (state: CleanRayState, _: Int32) =>
        val pos = vec3(state.px, state.py, state.pz)
        val rayDir = vec3(state.dx, state.dy, state.dz)
        val color = vec3(state.cr, state.cg, state.cb)
        val throughput = vec3(state.tr, state.tg, state.tb)

        when(state.done > 0.5f)(state).otherwise:
          when(state.dist > maxDist):
            // Ray escaped - add dark environment
            val env = darkEnvironment(rayDir, bass)
            CleanRayState(
              px = pos.x, py = pos.y, pz = pos.z,
              dx = rayDir.x, dy = rayDir.y, dz = rayDir.z,
              cr = color.x + throughput.x * env.x,
              cg = color.y + throughput.y * env.y,
              cb = color.z + throughput.z * env.z,
              tr = 0.0f, tg = 0.0f, tb = 0.0f,
              dist = state.dist,
              done = 1.0f,
            )
          .otherwise:
            // === EVALUATE BOTH SDFs SEPARATELY ===
            val waterDist = waterSurface(pos, time, bass, lowMid, highMid, treble)
            val poleDist = allPolesSdf(pos)
            
            // Step distance - minimum of both (safe marching)
            val stepDist = min(abs(waterDist), poleDist) * 0.9f
            val safeStep = max(stepDist, hitThreshold * 0.5f)
            
            // === HIT DETECTION ===
            // Water hit: close to surface OR crossed through (negative distance)
            val waterClose = abs(waterDist) < hitThreshold
            val waterCrossed = waterDist < -hitThreshold * 0.5f
            val waterHit = waterClose || waterCrossed
            
            // Pole hit with AA
            val poleAA = max(state.dist * pixelScale * 2.0f, hitThreshold)
            val poleHit = poleDist < poleAA
            val poleCoverage = when(poleHit):
              val t = clamp(poleDist / poleAA, 0.0f, 1.0f)
              1.0f - t * t
            .otherwise(0.0f)
            
            // === WATER SURFACE SHADING ===
            val waterN = when(waterHit):
              Raymarch.normal(pos, p => waterSurface(p, time, bass, lowMid, highMid, treble))
            .otherwise(vec3(0.0f, 1.0f, 0.0f))
            
            // Reflection
            val reflDir: Vec3[Float32] = Raymarch.reflect(rayDir, waterN)
            val reflOrigin = vec3(pos.x + waterN.x * 0.02f, pos.y + waterN.y * 0.02f, pos.z + waterN.z * 0.02f)
            val reflectedPoles = traceReflection(reflOrigin, reflDir)
            val reflectedEnv = darkEnvironment(reflDir, bass)
            
            // Fresnel - more reflection at grazing angles
            val negRayDir = vec3(-rayDir.x, -rayDir.y, -rayDir.z)
            val cosTheta = max(negRayDir dot waterN, 0.0f)
            val oneMinusCos = 1.0f - cosTheta
            val fresnelR = 0.04f + 0.96f * oneMinusCos * oneMinusCos * oneMinusCos * oneMinusCos * oneMinusCos
            
            // Water surface color contribution - very dark, almost black
            val waterBaseColor = vec3(0.005f, 0.006f, 0.01f)  // Nearly black with hint of blue
            val waterReflection = vec3(
              reflectedPoles.x + reflectedEnv.x * 0.3f,
              reflectedPoles.y + reflectedEnv.y * 0.3f,
              reflectedPoles.z + reflectedEnv.z * 0.3f,
            )
            
            val waterColorContrib = when(waterHit && !poleHit):
              // Darker reflections, lower intensity
              val reflContribX = waterBaseColor.x + waterReflection.x * fresnelR * 0.7f
              val reflContribY = waterBaseColor.y + waterReflection.y * fresnelR * 0.7f
              val reflContribZ = waterBaseColor.z + waterReflection.z * fresnelR * 0.7f
              vec3(
                throughput.x * reflContribX,
                throughput.y * reflContribY,
                throughput.z * reflContribZ,
              )
            .otherwise(vec3(0.0f, 0.0f, 0.0f))
            
            // Water is fully opaque - no light passes through
            val throughputAfterWater = when(waterHit):
              vec3(0.0f, 0.0f, 0.0f)  // Completely blocks light
            .otherwise(throughput)
            
            // === POLE SHADING ===
            val avgLevel = (bass + lowMid + highMid + treble) / 4.0f
            val poleEmission = vec3(1.0f + avgLevel * 0.5f, 1.0f, 1.0f + avgLevel * 0.3f)
            
            val poleColorContrib = when(poleHit):
              vec3(
                poleCoverage * poleEmission.x * throughputAfterWater.x,
                poleCoverage * poleEmission.y * throughputAfterWater.y,
                poleCoverage * poleEmission.z * throughputAfterWater.z,
              )
            .otherwise(vec3(0.0f, 0.0f, 0.0f))
            
            // === COMBINE RESULTS ===
            val newColor = vec3(
              color.x + waterColorContrib.x + poleColorContrib.x,
              color.y + waterColorContrib.y + poleColorContrib.y,
              color.z + waterColorContrib.z + poleColorContrib.z,
            )
            
            val newThroughput = when(poleHit):
              vec3(
                throughputAfterWater.x * (1.0f - poleCoverage),
                throughputAfterWater.y * (1.0f - poleCoverage),
                throughputAfterWater.z * (1.0f - poleCoverage),
              )
            .otherwise(throughputAfterWater)
            
            // === ADVANCE RAY ===
            val newPos = pos + rayDir * safeStep
            
            // Done if pole is solid hit OR throughput is depleted
            val throughputLeft = newThroughput.x + newThroughput.y + newThroughput.z
            val isDone = when((poleHit && poleCoverage > 0.95f) || (throughputLeft < 0.01f)):
              1.0f
            .otherwise(0.0f)

            CleanRayState(
              px = newPos.x, py = newPos.y, pz = newPos.z,
              dx = rayDir.x, dy = rayDir.y, dz = rayDir.z,
              cr = newColor.x, cg = newColor.y, cb = newColor.z,
              tr = newThroughput.x, tg = newThroughput.y, tb = newThroughput.z,
              dist = state.dist + safeStep,
              done = isDone,
            )
      )

    // Final color: add environment if ray didn't terminate
    val finalColor = when(result.done < 0.5f):
      val env = darkEnvironment(vec3(result.dx, result.dy, result.dz), bass)
      vec3(
        result.cr + result.tr * env.x,
        result.cg + result.tg * env.y,
        result.cb + result.tb * env.z,
      )
    .otherwise(vec3(result.cr, result.cg, result.cb))

    Raymarch.MarchResult(vec3(
      clamp(finalColor.x, 0.0f, 1.0f),
      clamp(finalColor.y, 0.0f, 1.0f),
      clamp(finalColor.z, 0.0f, 1.0f),
    ))

  def renderProgram(runner: AudioSdfRunner[Config]): GProgram[Int, runner.AudioSdfLayout] =
    GProgram.static[Int, runner.AudioSdfLayout](
      layout = n => runner.AudioSdfLayout(
        GBuffer[Vec4[Float16]](n),
        GUniform[Config](),
      ),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read

      // Band levels are pre-computed on CPU and passed as uniforms
      val bass = cfg.bass
      val lowMid = cfg.lowMid
      val highMid = cfg.highMid
      val treble = cfg.treble

      val materialPack = MaterialPack(
        mat0 = WATER_MAT.toProps,
        mat1 = Material.Solid.toProps,
        mat2 = POLE_MAT.toProps,
        mat3 = Material.Empty.toProps,
      )

      GIO.when(invocId < cfg.width.asInt * cfg.height.asInt):
        val px = invocId.mod(cfg.width.asInt)
        val py = invocId / cfg.width.asInt
        val u = (px.asFloat / cfg.width) * 2.0f - 1.0f
        val v = 1.0f - (py.asFloat / cfg.height) * 2.0f

        val camAngle = 0.35f
        val camPos = vec3(0.0f, 3.5f, -cfg.camDist)

        val cosA = cos(camAngle)
        val sinA = sin(camAngle)

        val aspect = cfg.width / cfg.height
        val rawDir = normalize(vec3f(u * aspect, v, cfg.fovDist))

        val rayDir = normalize(vec3(
          rawDir.x,
          rawDir.y * cosA - rawDir.z * sinA,
          rawDir.y * sinA + rawDir.z * cosA,
        ))

        val pixelScale = 2.0f / (cfg.fovDist * cfg.height)

        val result = marchWithGlow(
          origin = camPos,
          dir = rayDir,
          time = cfg.time,
          bass = bass,
          lowMid = lowMid,
          highMid = highMid,
          treble = treble,
          maxSteps = 120,  // More steps for better quality
          maxDist = 50.0f,
          hitThreshold = 0.003f,  // Tighter threshold
          pixelScale = pixelScale,
        )

        // Bloom effect - audio reactive
        val bloomSample1 = poleGlow(camPos + rayDir * 5.0f)
        val bloomSample2 = poleGlow(camPos + rayDir * 10.0f)
        val bloomSample3 = poleGlow(camPos + rayDir * 15.0f)

        val offsetAmount = 0.3f
        val rightOffset = normalize(vec3(rayDir.z, 0.0f, -rayDir.x)) * offsetAmount
        val upOffset = vec3(0.0f, offsetAmount, 0.0f)

        val bloomSample4 = poleGlow(camPos + rayDir * 8.0f + rightOffset)
        val bloomSample5 = poleGlow(camPos + rayDir * 8.0f - rightOffset)
        val bloomSample6 = poleGlow(camPos + rayDir * 8.0f + upOffset)
        val bloomSample7 = poleGlow(camPos + rayDir * 8.0f - upOffset)

        val bloomGlow = (bloomSample1 * 0.3f + bloomSample2 * 0.25f + bloomSample3 * 0.15f +
          bloomSample4 * 0.1f + bloomSample5 * 0.1f +
          bloomSample6 * 0.05f + bloomSample7 * 0.05f) * 0.025f

        val finalColor = vec3(
          clamp(result.color.x + bloomGlow.x, 0.0f, 1.0f),
          clamp(result.color.y + bloomGlow.y, 0.0f, 1.0f),
          clamp(result.color.z + bloomGlow.z, 0.0f, 1.0f),
        )

        GIO.write(layout.pixels, invocId, Pixels.rgba16f(finalColor))

  /**
   * Run the music visualizer with default audio input.
   */
  @main def runMusicVisualizer(): Unit = runMusicVisualizerWithMixer("")

  /**
   * Run the music visualizer with a specific audio mixer.
   *
   * @param audioMixerName Audio mixer name. Use empty string for default input,
   *                       or specify a name like "stereo mix" for system audio loopback.
   */
  @main def runMusicVisualizerWithMixer(audioMixerName: String): Unit =
    println("Music Visualizer - CPU FFT, GPU Rendering")
    println("==========================================")
    println("Press Ctrl+C to exit")
    println()

    val mixerOpt = if audioMixerName.isEmpty then None else Some(audioMixerName)

    val runner = AudioSdfRunner[Config](WIDTH, HEIGHT, "Music Visualizer")

    runner.run(
      program = renderProgram(runner),
      makeConfig = ctx =>
        Config(
          time = ctx.time,
          width = ctx.width.toFloat,
          height = ctx.height.toFloat,
          camDist = CAM_DIST,
          fovDist = FOV_DIST,
          bass = ctx.bass,
          lowMid = ctx.lowMid,
          highMid = ctx.highMid,
          treble = ctx.treble,
        ),
      postProcess = Seq(
        AudioSdfRunner.filmLook[Config](
          runner = runner,
          grainStrength = 0.025f,
          vignetteStrength = 0.25f,
          contrast = 1.08f,
          saturation = 0.95f,
        )(
          getWidth = _.width.asInt,
          getHeight = _.height.asInt,
          getTime = _.time,
        ),
      ),
      audioMixerName = mixerOpt,
    )
