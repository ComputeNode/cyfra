package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}

/**
 * Unified raymarching with path-tracing style color accumulation.
 *
 * Uses throughput/color accumulation pattern:
 * - Accumulated color: final color being built up
 * - Throughput: how much light can still pass through (decreases at each surface)
 */
object Raymarch:

  /**
   * Ray state with color accumulation (path tracing pattern).
   *
   * @param posX/Y/Z Current ray position
   * @param dirX/Y/Z Current ray direction (may change on refraction)
   * @param colorR/G/B Accumulated color so far
   * @param throughputR/G/B Remaining light contribution factor
   * @param totalDist Total distance traveled
   * @param insideGlass 1.0 if currently inside glass, 0.0 if outside
   * @param glassPathLen Distance traveled through glass (for absorption)
   * @param done 1.0 if ray is finished (hit solid or missed)
   */
  case class RayState(
    posX: Float32, posY: Float32, posZ: Float32,
    dirX: Float32, dirY: Float32, dirZ: Float32,
    colorR: Float32, colorG: Float32, colorB: Float32,
    throughputR: Float32, throughputG: Float32, throughputB: Float32,
    totalDist: Float32,
    insideGlass: Float32,
    glassPathLen: Float32,
    done: Float32,
  ) extends GStruct[RayState]

  object RayState:
    given GStructSchema[RayState] = GStructSchema.derived

  // ---- Configuration ----
  val DefaultMaxSteps: Int = 256
  val DefaultMaxDist: Float = 50.0f
  val DefaultHitThreshold: Float = 0.001f
  val DefaultStepScale: Float = 0.8f

  // ---- Result ----

  /** Final color from raymarching. */
  case class MarchResult(color: Vec3[Float32])

  // ---- Core Raymarching with Accumulation ----

  /**
   * March a ray, accumulating color at each surface interaction.
   *
   * At glass surfaces: add reflection, multiply throughput by transmission, refract.
   * At solid surfaces: add shaded color, terminate.
   * On miss: add environment, terminate.
   */
  def march(
    origin: Vec3[Float32],
    dir: Vec3[Float32],
    sdf: Vec3[Float32] => SdfResult,
    materials: MaterialPack,
    lightDir: Vec3[Float32],
    time: Float32 = 0.0f,
    maxSteps: Int = DefaultMaxSteps,
    maxDist: Float32 = DefaultMaxDist,
    hitThreshold: Float32 = DefaultHitThreshold,
    stepScale: Float32 = DefaultStepScale,
  ): MarchResult =
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

    val result = GSeq.gen[Int32](0, _ + 1).limit(maxSteps)
      .fold(initState, (state: RayState, _: Int32) =>
        val pos = vec3(state.posX, state.posY, state.posZ)
        val rayD = vec3(state.dirX, state.dirY, state.dirZ)
        val color = vec3(state.colorR, state.colorG, state.colorB)
        val throughput = vec3(state.throughputR, state.throughputG, state.throughputB)
        val totalDist = state.totalDist
        val insideGlass = state.insideGlass
        val glassPath = state.glassPathLen

        // Early exit if done
        when(state.done > 0.5f)(state).otherwise:
          // Check if we've exceeded max distance (miss)
          when(totalDist > maxDist):
            // Apply absorption for any remaining glass path, then add environment
            val absorption = beerAbsorption(glassPath, materials.mat0)
            val env = sampleEnvironment(rayD, time)
            val finalColor = vec3(
              color.x + throughput.x * absorption.x * env.x,
              color.y + throughput.y * absorption.y * env.y,
              color.z + throughput.z * absorption.z * env.z,
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
            // Use absolute distance for marching (works both inside and outside)
            val signedDist = evalResult.distance
            val d = abs(signedDist)
            val mats = evalResult.materials

            // Get material properties from dominant material
            val matColor = getMaterialColor(materials, mats)
            val isTransparent = getMaterialProperty(materials, mats, _.isTransparent)
            val ior = getMaterialProperty(materials, mats, _.ior)
            val absorption = getMaterialProperty(materials, mats, _.absorption)
            val roughness = getMaterialProperty(materials, mats, _.roughness)

            // Detect surface hit
            val isHit = d < hitThreshold
            val isGlass = isTransparent > 0.5f
            val isGlassHit = isHit && isGlass
            val isSolidHit = isHit && !isGlass

            // Normal calculation (only when hit)
            // Use smooth normal for glass (avoids high-frequency refraction artifacts)
            // Use regular normal for solid surfaces (preserves detail for shading)
            val regularN = when(isHit)(normal(pos, p => sdf(p).distance)).otherwise(vec3(0.0f, 1.0f, 0.0f))
            val glassN = when(isHit && isGlass)(smoothNormal(pos, p => sdf(p).distance)).otherwise(regularN)
            val n = when(isGlass)(glassN).otherwise(regularN)

            // Flip normal if inside
            val orientedN = when(insideGlass > 0.5f)(-n).otherwise(n)

            // View direction
            val viewDir = vec3(-rayD.x, -rayD.y, -rayD.z)
            val cosTheta = max(viewDir dot orientedN, 0.0f)

            // === GLASS SURFACE HIT ===
            // Entering or exiting glass
            val entering = insideGlass < 0.5f
            val eta = when(entering)(1.0f / ior).otherwise(ior)
            val fresnelFactor = fresnel(cosTheta, ior)

            // Reflection contribution (add to color)
            val reflectDir = reflect(rayD, orientedN)
            val reflectedEnv = sampleEnvironment(reflectDir, time)
            val halfVec = normalize(lightDir + viewDir)
            val specNdotH = max(orientedN dot halfVec, 0.0f)
            val specular = pow(specNdotH, 256.0f) * 1.2f

            // When exiting glass, apply absorption for the path traveled
            val exitAbsorption = when(entering)(vec3(1.0f, 1.0f, 1.0f)).otherwise(beerAbsorption(glassPath, materials.mat0))

            val glassReflectionColor = when(isGlassHit):
              val reflectionIntensity = fresnelFactor * 0.8f // Reduce reflection intensity
              vec3(
                color.x + throughput.x * (reflectedEnv.x + specular) * reflectionIntensity * exitAbsorption.x,
                color.y + throughput.y * (reflectedEnv.y + specular) * reflectionIntensity * exitAbsorption.y,
                color.z + throughput.z * (reflectedEnv.z + specular * 0.9f) * reflectionIntensity * exitAbsorption.z,
              )
            .otherwise(color)

            // Throughput after glass hit (transmission * absorption for exit)
            val transmission = 1.0f - fresnelFactor * 0.8f
            val glassThroughput = when(isGlassHit):
              vec3(
                throughput.x * transmission * exitAbsorption.x,
                throughput.y * transmission * exitAbsorption.y,
                throughput.z * transmission * exitAbsorption.z,
              )
            .otherwise(throughput)

            // Refraction
            val refractResult = refract(rayD, orientedN, eta)
            val refractDir = normalize(vec3(refractResult.x, refractResult.y, refractResult.z))
            val tir = refractResult.w > 0.5f

            // If total internal reflection, reflect instead
            val glassNewDir = when(isGlassHit):
              when(tir)(reflectDir).otherwise(refractDir)
            .otherwise(rayD)

            // Update inside/outside state
            val newInsideGlass = when(isGlassHit):
              when(tir)(insideGlass).otherwise(when(entering)(1.0f).otherwise(0.0f))
            .otherwise(insideGlass)

            // Reset glass path on exit, keep accumulating inside
            val newGlassPath = when(isGlassHit):
              when(entering)(0.0f).otherwise(0.0f) // Reset on any glass surface
            .otherwise:
              when(insideGlass > 0.5f)(glassPath + d).otherwise(0.0f)

            // === SOLID SURFACE HIT ===
            val solidColor = when(isSolidHit):
              // Main light
              val ndotl = max(n dot lightDir, 0.0f)

              // Fill light from below (for ceiling illumination)
              val fillLightDir = vec3(0.0f, -1.0f, 0.0f)
              val ndotFill = max(n dot fillLightDir, 0.0f) * 0.4f

              // Hemisphere ambient (sky-like)
              val hemisphereAmbient = 0.5f + n.y * 0.1f

              // Combined diffuse
              val diffuse = ndotl * 0.5f + ndotFill + hemisphereAmbient

              val halfVec = normalize(lightDir + viewDir)
              val specNdotH = max(n dot halfVec, 0.0f)
              val specPower = 64.0f * (1.0f - roughness) + 8.0f
              val specular = pow(specNdotH, specPower) * 0.3f

              val shaded = vec3(
                matColor.x * diffuse + specular,
                matColor.y * diffuse + specular,
                matColor.z * diffuse + specular * 0.9f,
              )

              vec3(
                glassReflectionColor.x + glassThroughput.x * shaded.x,
                glassReflectionColor.y + glassThroughput.y * shaded.y,
                glassReflectionColor.z + glassThroughput.z * shaded.z,
              )
            .otherwise(glassReflectionColor)

            // Final state updates
            val finalColor = solidColor
            val finalThroughput = when(isSolidHit)(vec3(0.0f, 0.0f, 0.0f)).otherwise(glassThroughput)

            // Step forward - use larger offset for glass to properly escape displaced surfaces
            val stepSize = max(d * stepScale, hitThreshold)
            val glassOffset = 0.12f // Large offset to escape spiky/displaced glass surfaces
            val offset = when(isGlassHit)(glassOffset).otherwise(stepSize)
            val newDir = when(isGlassHit)(glassNewDir).otherwise(rayD)
            val newPos = pos + newDir * offset
            val newDist = totalDist + offset

            RayState(
              posX = newPos.x, posY = newPos.y, posZ = newPos.z,
              dirX = newDir.x, dirY = newDir.y, dirZ = newDir.z,
              colorR = finalColor.x, colorG = finalColor.y, colorB = finalColor.z,
              throughputR = finalThroughput.x, throughputG = finalThroughput.y, throughputB = finalThroughput.z,
              totalDist = newDist,
              insideGlass = newInsideGlass,
              glassPathLen = newGlassPath,
              done = when(isSolidHit)(1.0f).otherwise(0.0f),
            )
      )

    // If not done (ran out of steps or exited scene), add environment
    val finalColor = when(result.done < 0.5f):
      val absorption = when(result.insideGlass > 0.5f)(beerAbsorption(result.glassPathLen, materials.mat0)).otherwise(vec3(1.0f, 1.0f, 1.0f))
      val env = sampleEnvironment(vec3(result.dirX, result.dirY, result.dirZ), time)
      vec3(
        result.colorR + result.throughputR * absorption.x * env.x,
        result.colorG + result.throughputG * absorption.y * env.y,
        result.colorB + result.throughputB * absorption.z * env.z,
      )
    .otherwise(vec3(result.colorR, result.colorG, result.colorB))

    MarchResult(vec3(
      clamp(finalColor.x, 0.0f, 1.0f),
      clamp(finalColor.y, 0.0f, 1.0f),
      clamp(finalColor.z, 0.0f, 1.0f),
    ))

  // ---- Beer's Law Absorption ----

  def beerAbsorption(distance: Float32, mat: MaterialProps): Vec3[Float32] =
    val factor = -distance * mat.absorption
    vec3(
      exp(factor * (1.0f - mat.colorR)),
      exp(factor * (1.0f - mat.colorG)),
      exp(factor * (1.0f - mat.colorB)),
    )

  // ---- Surface Calculations ----

  /** Standard normal via central differences. */
  def normal(p: Vec3[Float32], sdf: Vec3[Float32] => Float32, eps: Float32 = 0.002f): Vec3[Float32] =
    val nx = sdf(vec3(p.x + eps, p.y, p.z)) - sdf(vec3(p.x - eps, p.y, p.z))
    val ny = sdf(vec3(p.x, p.y + eps, p.z)) - sdf(vec3(p.x, p.y - eps, p.z))
    val nz = sdf(vec3(p.x, p.y, p.z + eps)) - sdf(vec3(p.x, p.y, p.z - eps))
    normalize(vec3(nx, ny, nz))

  /** Smooth normal using larger epsilon - for glass refraction to avoid high-frequency artifacts. */
  def smoothNormal(p: Vec3[Float32], sdf: Vec3[Float32] => Float32): Vec3[Float32] =
    normal(p, sdf, eps = 0.15f) // Much larger epsilon smooths out displacement details

  // ---- Optics ----

  def refract(incident: Vec3[Float32], normal: Vec3[Float32], eta: Float32): Vec4[Float32] =
    val cosi = -(incident dot normal)
    val k = 1.0f - eta * eta * (1.0f - cosi * cosi)
    val tir = when(k < 0.0f)(1.0f).otherwise(0.0f)
    val sqrtK = sqrt(max(k, 0.0f))
    val refracted = normalize(incident * eta + normal * (eta * cosi - sqrtK))
    vec4(refracted.x, refracted.y, refracted.z, tir)

  def reflect(incident: Vec3[Float32], normal: Vec3[Float32]): Vec3[Float32] =
    incident - normal * (2.0f * (incident dot normal))

  def fresnel(cosTheta: Float32, ior: Float32): Float32 =
    val r0Base = (1.0f - ior) / (1.0f + ior)
    val r0 = r0Base * r0Base
    val x = 1.0f - cosTheta
    val x2 = x * x
    val x5 = x2 * x2 * x
    r0 + (1.0f - r0) * x5

  // ---- Material Helpers ----

  def getMaterialColor(materials: MaterialPack, intensities: UInt32): Vec3[Float32] =
    val i0 = SdfResult.intensityNormalized(intensities, 0)
    val i1 = SdfResult.intensityNormalized(intensities, 1)
    val i2 = SdfResult.intensityNormalized(intensities, 2)
    val i3 = SdfResult.intensityNormalized(intensities, 3)
    val total = i0 + i1 + i2 + i3 + 0.001f
    vec3(
      (materials.mat0.colorR * i0 + materials.mat1.colorR * i1 + materials.mat2.colorR * i2 + materials.mat3.colorR * i3) / total,
      (materials.mat0.colorG * i0 + materials.mat1.colorG * i1 + materials.mat2.colorG * i2 + materials.mat3.colorG * i3) / total,
      (materials.mat0.colorB * i0 + materials.mat1.colorB * i1 + materials.mat2.colorB * i2 + materials.mat3.colorB * i3) / total,
    )

  def getMaterialProperty(materials: MaterialPack, intensities: UInt32, getter: MaterialProps => Float32): Float32 =
    val i0 = SdfResult.intensityNormalized(intensities, 0)
    val i1 = SdfResult.intensityNormalized(intensities, 1)
    val i2 = SdfResult.intensityNormalized(intensities, 2)
    val i3 = SdfResult.intensityNormalized(intensities, 3)
    val total = i0 + i1 + i2 + i3 + 0.001f
    (getter(materials.mat0) * i0 + getter(materials.mat1) * i1 + getter(materials.mat2) * i2 + getter(materials.mat3) * i3) / total

  // ---- Environment ----

  def sampleEnvironment(dir: Vec3[Float32], time: Float32 = 0.0f): Vec3[Float32] =
    val y = dir.y
    val skyT = clamp(y * 0.5f + 0.5f, 0.0f, 1.0f)
    val horizonT = exp(-abs(y) * 3.0f)
    val skyBlue = vec3(0.4f, 0.6f, 0.9f)
    val skyWhite = vec3(0.9f, 0.95f, 1.0f)
    val horizon = vec3(1.0f, 0.95f, 0.85f)
    val sky = skyBlue * (1.0f - skyT) + skyWhite * skyT
    val withHorizon = sky * (1.0f - horizonT * 0.3f) + horizon * horizonT * 0.3f
    val angle = dir.x * 2.0f + dir.z * 1.5f + time * 0.1f
    val variation = sin(angle * 3.0f) * 0.02f
    vec3(
      clamp(withHorizon.x + variation, 0.0f, 1.0f),
      clamp(withHorizon.y + variation * 0.8f, 0.0f, 1.0f),
      clamp(withHorizon.z + variation * 0.5f, 0.0f, 1.0f),
    )
