package io.computenode.samples.cyfra.oldsamples

import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.utility.ImageUtility

import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

given GContext = new GContext()
given ExecutionContext = Implicits.global

/** Raytracing example
  */

@main
def main =

  val dim = 2048
  val minRayHitTime = 0.01f
  val rayPosNormalNudge = 0.01f
  val superFar = 1000.0f
  val fovDeg = 60
  val fovRad = fovDeg * math.Pi.toFloat / 180.0f
  val maxBounces = 8
  val pixelIterationsPerFrame = 1000
  val bgColor = (0.2f, 0.2f, 0.2f)
  val exposure = 1f

  case class Random[T <: Value](value: T, nextSeed: UInt32)

  def lessThan(f: Vec3[Float32], f2: Float32): Vec3[Float32] =
    (when(f.x < f2)(1.0f).otherwise(0.0f), when(f.y < f2)(1.0f).otherwise(0.0f), when(f.z < f2)(1.0f).otherwise(0.0f))

  def linearToSRGB(rgb: Vec3[Float32]): Vec3[Float32] =
    val clampedRgb = vclamp(rgb, 0.0f, 1.0f)
    mix(pow(clampedRgb, vec3(1.0f / 2.4f)) * 1.055f - vec3(0.055f), clampedRgb * 12.92f, lessThan(clampedRgb, 0.0031308f))

  def SRGBToLinear(rgb: Vec3[Float32]): Vec3[Float32] =
    val clampedRgb = vclamp(rgb, 0.0f, 1.0f)
    mix(pow((clampedRgb + vec3(0.055f)) * (1.0f / 1.055f), vec3(2.4f)), clampedRgb * (1.0f / 12.92f), lessThan(clampedRgb, 0.04045f))

  def ACESFilm(x: Vec3[Float32]): Vec3[Float32] =
    val a = 2.51f
    val b = 0.03f
    val c = 2.43f
    val d = 0.59f
    val e = 0.14f
    vclamp((x mulV (x * a + vec3(b))) divV (x mulV (x * c + vec3(d)) + vec3(e)), 0.0f, 1.0f)

  case class RayHitInfo(
    dist: Float32,
    normal: Vec3[Float32],
    albedo: Vec3[Float32],
    emissive: Vec3[Float32],
    percentSpecular: Float32 = 0f,
    roughness: Float32 = 0f,
    specularColor: Vec3[Float32] = vec3(0f),
    indexOfRefraction: Float32 = 1.0f,
    refractionChance: Float32 = 0f,
    refractionRoughness: Float32 = 0f,
    refractionColor: Vec3[Float32] = vec3(0f),
    fromInside: GBoolean = false,
  ) extends GStruct[RayHitInfo]

  case class Sphere(
    center: Vec3[Float32],
    radius: Float32,
    color: Vec3[Float32],
    emissive: Vec3[Float32],
    percentSpecular: Float32 = 0f,
    roughness: Float32 = 0f,
    specularColor: Vec3[Float32] = vec3(0f),
    indexOfRefraction: Float32 = 1f,
    refractionChance: Float32 = 0f,
    refractionRoughness: Float32 = 0f,
    refractionColor: Vec3[Float32] = vec3(0f),
  ) extends GStruct[Sphere]

  case class Quad(
    a: Vec3[Float32],
    b: Vec3[Float32],
    c: Vec3[Float32],
    d: Vec3[Float32],
    color: Vec3[Float32],
    emissive: Vec3[Float32],
    percentSpecular: Float32 = 0f,
    roughness: Float32 = 0f,
    specularColor: Vec3[Float32] = vec3(0f),
    indexOfRefraction: Float32 = 1f,
    refractionChance: Float32 = 0f,
    refractionRoughness: Float32 = 0f,
    refractionColor: Vec3[Float32] = vec3(0f),
  ) extends GStruct[Quad]

  case class RayTraceState(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    color: Vec3[Float32],
    throughput: Vec3[Float32],
    rngState: UInt32,
    finished: GBoolean = false,
  ) extends GStruct[RayTraceState]

  val sceneTranslation = vec4(0f, 0f, 10f, 0f)
  // 7 is cool
  val rd = scala.util.Random(3)

  def scalaTwoSpheresIntersect(sphereA: (Float, Float, Float), radiusA: Float, sphereB: (Float, Float, Float), radiusB: Float): Boolean =
    val dist = Math.sqrt(
      (sphereA._1 - sphereB._1) *
        (sphereA._1 - sphereB._1) +
        (sphereA._2 - sphereB._2) *
        (sphereA._2 - sphereB._2) +
        (sphereA._3 - sphereB._3) *
        (sphereA._3 - sphereB._3),
    )
    dist < radiusA + radiusB

  val existingSpheres = mutable.Set.empty[((Float, Float, Float), Float)]
  @tailrec
  def randomSphere(iter: Int = 0): Sphere =
    if iter > 1000 then throw new Exception("Could not find a non-intersecting sphere")
    def nextFloatAny = rd.nextFloat() * 2f - 1f

    def nextFloatPos = rd.nextFloat()

    val center = (nextFloatAny * 10, nextFloatAny * 10, nextFloatPos * 10 + 8f)
    val radius = nextFloatPos + 1.5f
    if existingSpheres.exists(s => scalaTwoSpheresIntersect(s._1, s._2, center, radius)) then randomSphere(iter + 1)
    else
      existingSpheres.add((center, radius))
      def color = (nextFloatPos * 0.5f + 0.5f, nextFloatPos * 0.5f + 0.5f, nextFloatPos * 0.5f + 0.5f)
      val emissive = (0f, 0f, 0f)
      Sphere(
        center,
        radius,
        color,
        emissive,
        0.45f,
        0.1f,
        (nextFloatPos + 0.2f, nextFloatPos + 0.2f, nextFloatPos + 0.2f),
        1.1f,
        0.6f,
        0.1f,
        (nextFloatPos, nextFloatPos, nextFloatPos),
      )

  def randomSpheres(n: Int) = List.fill(n)(randomSphere())

  val flash = // flash
    val x = -10f
    val mX = -5f
    val y = -10f
    val mY = 0f
    val z = -5f
    Sphere((-7.5f, -12f, -5f), 3f, (1f, 1f, 1f), (20f, 20f, 20f))
  val spheres = (flash :: randomSpheres(20)).map(sp => sp.copy(center = sp.center + sceneTranslation.xyz))

  val walls = List(
    Quad( // back
      (-15.5f, -15.5f, 25.0f),
      (15.5f, -15.5f, 25.0f),
      (15.5f, 15.5f, 25.0f),
      (-15.5f, 15.5f, 25.0f),
      (0.8f, 0.8f, 0.8f),
      (0f, 0f, 0f),
    ),
    Quad( // right
      (15f, -15.5f, 25.5f),
      (15f, -15.5f, -15.5f),
      (15f, 15.5f, -15.5f),
      (15f, 15.5f, 25.5f),
      (0.0f, 0.8f, 0.0f),
      (0f, 0f, 0f),
    ),
    Quad( // left
      (-15f, -15.5f, 25.5f),
      (-15f, -15.5f, -15.5f),
      (-15f, 15.5f, -15.5f),
      (-15f, 15.5f, 25.5f),
      (0.8f, 0.0f, 0.0f),
      (0f, 0f, 0f),
    ),
    Quad( // bottom
      (-15.5f, 15f, 25.5f),
      (15.5f, 15f, 25.5f),
      (15.5f, 15f, -15.5f),
      (-15.5f, 15f, -15.5f),
      (0.8f, 0.8f, 0.8f),
      (0f, 0f, 0f),
    ),
    Quad( // top
      (-15.5f, -15f, 25.5f),
      (15.5f, -15f, 25.5f),
      (15.5f, -15f, -15.5f),
      (-15.5f, -15f, -15.5f),
      (0.8f, 0.8f, 0.8f),
      (0f, 0f, 0f),
    ),
    Quad( // front
      (-15.5f, -15.5f, -15.5f),
      (15.5f, -15.5f, -15.5f),
      (15.5f, 15.5f, -15.5f),
      (-15.5f, 15.5f, -15.5f),
      (0.8f, 0.8f, 0.8f),
      (0f, 0f, 0f),
    ),
    Quad( // light
      (-2.5f, -14.95f, 17.5f),
      (2.5f, -14.95f, 17.5f),
      (2.5f, -14.95f, 12.5f),
      (-2.5f, -14.95f, 12.5f),
      (1f, 1f, 1f),
      (20f, 18f, 14f),
    ),
  ).map(quad =>
    quad.copy(
      a = quad.a + sceneTranslation.xyz,
      b = quad.b + sceneTranslation.xyz,
      c = quad.c + sceneTranslation.xyz,
      d = quad.d + sceneTranslation.xyz,
    ),
  )

  case class RaytracingIteration(frame: Int32) extends GStruct[RaytracingIteration]

  def function(): GFunction[RaytracingIteration, Vec4[Float32], Vec4[Float32]] = GFunction.from2D(dim):
    case (RaytracingIteration(frame), (xi: Int32, yi: Int32), lastFrame) =>
      def wangHash(seed: UInt32): UInt32 =
        val s1 = (seed ^ 61) ^ (seed >> 16)
        val s2 = s1 * 9
        val s3 = s2 ^ (s2 >> 4)
        val s4 = s3 * 0x27d4eb2d
        s4 ^ (s4 >> 15)

      def randomFloat(seed: UInt32): Random[Float32] =
        val nextSeed = wangHash(seed)
        val f = nextSeed.asFloat / 4294967296.0f
        Random(f, nextSeed)

      def randomVector(seed: UInt32): Random[Vec3[Float32]] =
        val Random(z, seed1) = randomFloat(seed)
        val z2 = z * 2.0f - 1.0f
        val Random(a, seed2) = randomFloat(seed1)
        val a2 = a * 2.0f * math.Pi.toFloat
        val r = sqrt(1.0f - z2 * z2)
        val x = r * cos(a2)
        val y = r * sin(a2)
        Random((x, y, z2), seed2)

      def scalarTriple(u: Vec3[Float32], v: Vec3[Float32], w: Vec3[Float32]): Float32 = (u cross v) dot w

      def testQuadTrace(rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo, quad: Quad): RayHitInfo =
        val normal = normalize((quad.c - quad.a) cross (quad.c - quad.b))
        val fixedQuad = when((normal dot rayDir) > 0f) {
          Quad(quad.d, quad.c, quad.b, quad.a, quad.color, quad.emissive)
        } otherwise quad
        val fixedNormal = when((normal dot rayDir) > 0f)(-normal).otherwise(normal)
        val p = rayPos
        val q = rayPos + rayDir
        val pq = q - p
        val pa = fixedQuad.a - p
        val pb = fixedQuad.b - p
        val pc = fixedQuad.c - p
        val m = pc cross pq
        val v = pa dot m

        def checkHit(intersectPoint: Vec3[Float32]): RayHitInfo =
          val dist = when(abs(rayDir.x) > 0.1f) {
            (intersectPoint.x - rayPos.x) / rayDir.x
          }.elseWhen(abs(rayDir.y) > 0.1f) {
            (intersectPoint.y - rayPos.y) / rayDir.y
          }.otherwise:
            (intersectPoint.z - rayPos.z) / rayDir.z
          when(dist > minRayHitTime && dist < currentHit.dist) {
            RayHitInfo(
              dist,
              fixedNormal,
              quad.color,
              quad.emissive,
              quad.percentSpecular,
              quad.roughness,
              quad.specularColor,
              quad.indexOfRefraction,
              quad.refractionChance,
              quad.refractionRoughness,
              quad.refractionColor,
            )
          } otherwise currentHit

        when(v >= 0f) {
          val u = -(pb dot m)
          val w = scalarTriple(pq, pb, pa)
          when(u >= 0f && w >= 0f) {
            val denom = 1f / (u + v + w)
            val uu = u * denom
            val vv = v * denom
            val ww = w * denom
            val intersectPos = fixedQuad.a * uu + fixedQuad.b * vv + fixedQuad.c * ww
            checkHit(intersectPos)
          } otherwise currentHit
        } otherwise:
          val pd = fixedQuad.d - p
          val u = pd dot m
          val w = scalarTriple(pq, pa, pd)
          when(u >= 0f && w >= 0f) {
            val negV = -v
            val denom = 1f / (u + negV + w)
            val uu = u * denom
            val vv = negV * denom
            val ww = w * denom
            val intersectPos = fixedQuad.a * uu + fixedQuad.d * vv + fixedQuad.c * ww
            checkHit(intersectPos)
          } otherwise currentHit

      def testSphereTrace(rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo, sphere: Sphere): RayHitInfo =
        val toRay = rayPos - sphere.center
        val b = toRay dot rayDir
        val c = (toRay dot toRay) - (sphere.radius * sphere.radius)
        val notHit = currentHit
        when(c > 0f && b > 0f) {
          notHit
        } otherwise:
          val discr = b * b - c
          when(discr > 0f) {
            val initDist = -b - sqrt(discr)
            val fromInside = initDist < 0f
            val dist = when(fromInside)(-b + sqrt(discr)).otherwise(initDist)
            when(dist > minRayHitTime && dist < currentHit.dist) {
              val normal = normalize((rayPos + rayDir * dist - sphere.center) * when(fromInside)(-1f).otherwise(1f))
              RayHitInfo(
                dist,
                normal,
                sphere.color,
                sphere.emissive,
                sphere.percentSpecular,
                sphere.roughness,
                sphere.specularColor,
                sphere.indexOfRefraction,
                sphere.refractionChance,
                sphere.refractionRoughness,
                sphere.refractionColor,
                fromInside,
              )
            } otherwise notHit
          } otherwise notHit

      def testScene(rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo): RayHitInfo =

        val spheresHit = GSeq
          .of(spheres)
          .fold(
            currentHit,
            { case (hit, sphere) =>
              testSphereTrace(rayPos, rayDir, hit, sphere)
            },
          )

        GSeq.of(walls).fold(spheresHit, (hit, wall) => testQuadTrace(rayPos, rayDir, hit, wall))

      def fresnelReflectAmount(n1: Float32, n2: Float32, normal: Vec3[Float32], incident: Vec3[Float32], f0: Float32, f90: Float32): Float32 =
        val r0 = ((n1 - n2) / (n1 + n2)) * ((n1 - n2) / (n1 + n2))
        val cosX = -(normal dot incident)
        when(n1 > n2) {
          val n = n1 / n2
          val sinT2 = n * n * (1f - cosX * cosX)
          when(sinT2 > 1f) {
            f90
          } otherwise:
            val cosX2 = sqrt(1.0f - sinT2)
            val x = 1.0f - cosX2
            val ret = r0 + ((1.0f - r0) * x * x * x * x * x)
            mix(f0, f90, ret)
        } otherwise:
          val x = 1.0f - cosX
          val ret = r0 + ((1.0f - r0) * x * x * x * x * x)
          mix(f0, f90, ret)

      val MaxBounces = 8
      def getColorForRay(startRayPos: Vec3[Float32], startRayDir: Vec3[Float32], initRngState: UInt32): RayTraceState =
        val initState = RayTraceState(startRayPos, startRayDir, (0f, 0f, 0f), (1f, 1f, 1f), initRngState)
        GSeq
          .gen[RayTraceState](
            first = initState,
            next = { case state @ RayTraceState(rayPos, rayDir, color, throughput, rngState, _) =>

              val noHit = RayHitInfo(superFar, (0f, 0f, 0f), (0f, 0f, 0f), (0f, 0f, 0f))
              val testResult = testScene(rayPos, rayDir, noHit)
              when(testResult.dist < superFar) {

                val throughput2 = when(testResult.fromInside) {
                  throughput mulV exp[Vec3[Float32]](-testResult.refractionColor * testResult.dist)
                }.otherwise {
                  throughput
                }

                val specularChance = when(testResult.percentSpecular > 0.0f) {
                  fresnelReflectAmount(
                    when(testResult.fromInside)(testResult.indexOfRefraction).otherwise(1.0f),
                    when(!testResult.fromInside)(testResult.indexOfRefraction).otherwise(1.0f),
                    rayDir,
                    testResult.normal,
                    testResult.percentSpecular,
                    1.0f,
                  )
                }.otherwise {
                  0f
                }

                val refractionChance = when(specularChance > 0.0f) {
                  testResult.refractionChance * ((1.0f - specularChance) / (1.0f - testResult.percentSpecular))
                } otherwise testResult.refractionChance

                val Random(rayRoll, nextRngState1) = randomFloat(rngState)
                val doSpecular = when(specularChance > 0.0f && rayRoll < specularChance) {
                  1.0f
                }.otherwise(0.0f)

                val doRefraction = when(refractionChance > 0.0f && doSpecular === 0.0f && rayRoll < specularChance + refractionChance) {
                  1.0f
                }.otherwise(0.0f)

                val rayProbability = when(doSpecular === 1.0f) {
                  specularChance
                }.elseWhen(doRefraction === 1.0f) {
                  refractionChance
                }.otherwise {
                  1.0f - (specularChance + refractionChance)
                }

                val rayProbabilityCorrected = max(rayProbability, 0.01f)

                val nextRayPos = when(doRefraction === 1.0f) {
                  (rayPos + rayDir * testResult.dist) - (testResult.normal * rayPosNormalNudge)
                }.otherwise {
                  (rayPos + rayDir * testResult.dist) + (testResult.normal * rayPosNormalNudge)
                }

                val Random(randomVec1, nextRngState2) = randomVector(nextRngState1)
                val diffuseRayDir = normalize(testResult.normal + randomVec1)
                val specularRayDirPerfect = reflect(rayDir, testResult.normal)
                val specularRayDir = normalize(mix(specularRayDirPerfect, diffuseRayDir, testResult.roughness * testResult.roughness))

                val Random(randomVec2, nextRngState3) = randomVector(nextRngState2)
                val refractionRayDirPerfect =
                  refract(
                    rayDir,
                    testResult.normal,
                    when(testResult.fromInside)(testResult.indexOfRefraction).otherwise(1.0f / testResult.indexOfRefraction),
                  )
                val refractionRayDir =
                  normalize(
                    mix(
                      refractionRayDirPerfect,
                      normalize(-testResult.normal + randomVec2),
                      testResult.refractionRoughness * testResult.refractionRoughness,
                    ),
                  )

                val rayDirSpecular = mix(diffuseRayDir, specularRayDir, doSpecular)
                val rayDirRefracted = mix(rayDirSpecular, refractionRayDir, doRefraction)

                val nextColor = (throughput2 mulV testResult.emissive) addV color

                val nextThroughput = when(doRefraction === 0.0f) {
                  throughput2 mulV mix[Vec3[Float32]](testResult.albedo, testResult.specularColor, doSpecular)
                }.otherwise(throughput2)

                val throughputRayProb = nextThroughput * (1.0f / rayProbabilityCorrected)

                RayTraceState(nextRayPos, rayDirRefracted, nextColor, throughputRayProb, nextRngState3)
              } otherwise RayTraceState(rayPos, rayDir, color, throughput, rngState, true)

            },
          )
          .limit(MaxBounces)
          .takeWhile(!_.finished)
          .lastOr(initState)

      val rngState = xi * 1973 + yi * 9277 + frame * 26699 | 1
      case class RenderIteration(color: Vec3[Float32], rngState: UInt32) extends GStruct[RenderIteration]
      val color =
        GSeq
          .gen(
            first = RenderIteration((0f, 0f, 0f), rngState.unsigned),
            next = { case RenderIteration(_, rngState) =>
              val Random(wiggleX, rngState1) = randomFloat(rngState)
              val Random(wiggleY, rngState2) = randomFloat(rngState1)
              val x = ((xi.asFloat + wiggleX) / dim.toFloat) * 2f - 1f
              val y = ((yi.asFloat + wiggleY) / dim.toFloat) * 2f - 1f
              val xy = (x, y)

              val rayPosition = (0f, 0f, 0f)
              val cameraDist = 1.0f / tan(fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
              val rayTarget = (x, y, cameraDist)

              val rayDir = normalize(rayTarget - rayPosition)
              val rtResult = getColorForRay(rayPosition, rayDir, rngState)
              val withBg = vclamp(rtResult.color + (SRGBToLinear(bgColor) mulV rtResult.throughput), 0.0f, 20.0f)
              RenderIteration(withBg, rtResult.rngState)
            },
          )
          .limit(pixelIterationsPerFrame)
          .fold((0f, 0f, 0f), { case (acc, RenderIteration(color, _)) => acc + (color * (1.0f / pixelIterationsPerFrame.toFloat)) })

      when(frame === 0) {
        (color, 1.0f)
      } otherwise mix(lastFrame.at(xi, yi), (color, 1.0f), vec4(1.0f / (frame.asFloat + 1f)))

  val initialMem = Array.fill(dim * dim)((0.5f, 0.5f, 0.5f, 0.5f))
  val renders = 100
  val code = function()
  List.range(0, renders).foldLeft(initialMem) { case (mem, i) =>
    UniformContext.withUniform(RaytracingIteration(i)):
      val newMem = Vec4FloatMem(mem).map(code).asInstanceOf[Vec4FloatMem].toArray
      ImageUtility.renderToImage(newMem, dim, Paths.get(s"generated.png"))
      println(s"Finished render $i")
      newMem
  }
