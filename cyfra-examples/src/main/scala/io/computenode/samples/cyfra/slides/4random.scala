package io.computenode.samples.cyfra.slides

import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.utility.ImageUtility

import java.nio.file.Paths

def wangHash(seed: UInt32): UInt32 =
  val s1 = (seed ^ 61) ^ (seed >> 16)
  val s2 = s1 * 9
  val s3 = s2 ^ (s2 >> 4)
  val s4 = s3 * 0x27d4eb2d
  s4 ^ (s4 >> 15)

case class Random[T <: Value](value: T, nextSeed: UInt32)

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

@main
def randomRays() =
  val raysPerPixel = 10
  val dim = 1024
  val fovDeg = 80
  val minRayHitTime = 0.01f
  val superFar = 999f
  val maxBounces = 10
  val rayPosNudge = 0.001f
  val pixelIterationsPerFrame = 20000

  def scalarTriple(u: Vec3[Float32], v: Vec3[Float32], w: Vec3[Float32]): Float32 = (u cross v) dot w

  case class Sphere(center: Vec3[Float32], radius: Float32, color: Vec3[Float32], emissive: Vec3[Float32]) extends GStruct[Sphere]

  case class Quad(a: Vec3[Float32], b: Vec3[Float32], c: Vec3[Float32], d: Vec3[Float32], color: Vec3[Float32], emissive: Vec3[Float32])
      extends GStruct[Quad]

  case class RayHitInfo(dist: Float32, normal: Vec3[Float32], albedo: Vec3[Float32], emissive: Vec3[Float32]) extends GStruct[RayHitInfo]

  case class RayTraceState(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    color: Vec3[Float32],
    throughput: Vec3[Float32],
    rngSeed: UInt32,
    finished: GBoolean = false,
  ) extends GStruct[RayTraceState]

  def testSphereTrace(rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo, sphere: Sphere): RayHitInfo =
    val toRay = rayPos - sphere.center
    val b = toRay dot rayDir
    val c = (toRay dot toRay) - (sphere.radius * sphere.radius)
    val notHit = currentHit
    when(c > 0f && b > 0f):
      notHit
    .otherwise:
      val discr = b * b - c
      when(discr > 0f):
        val initDist = -b - sqrt(discr)
        val fromInside = initDist < 0f
        val dist = when(fromInside)(-b + sqrt(discr)).otherwise(initDist)
        when(dist > minRayHitTime && dist < currentHit.dist):
          val normal = normalize(rayPos + rayDir * dist - sphere.center)
          RayHitInfo(dist, normal, sphere.color, sphere.emissive)
        .otherwise:
          notHit
      .otherwise:
        notHit

  def testQuadTrace(rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo, quad: Quad): RayHitInfo =
    val normal = normalize((quad.c - quad.a) cross (quad.c - quad.b))
    val fixedQuad = when((normal dot rayDir) > 0f):
      Quad(quad.d, quad.c, quad.b, quad.a, quad.color, quad.emissive)
    .otherwise:
      quad
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
      val dist = when(abs(rayDir.x) > 0.1f):
        (intersectPoint.x - rayPos.x) / rayDir.x
      .elseWhen(abs(rayDir.y) > 0.1f):
        (intersectPoint.y - rayPos.y) / rayDir.y
      .otherwise:
        (intersectPoint.z - rayPos.z) / rayDir.z
      when(dist > minRayHitTime && dist < currentHit.dist):
        RayHitInfo(dist, fixedNormal, quad.color, quad.emissive)
      .otherwise:
        currentHit

    when(v >= 0f):
      val u = -(pb dot m)
      val w = scalarTriple(pq, pb, pa)
      when(u >= 0f && w >= 0f):
        val denom = 1f / (u + v + w)
        val uu = u * denom
        val vv = v * denom
        val ww = w * denom
        val intersectPos = fixedQuad.a * uu + fixedQuad.b * vv + fixedQuad.c * ww
        checkHit(intersectPos)
      .otherwise:
        currentHit
    .otherwise:
      val pd = fixedQuad.d - p
      val u = pd dot m
      val w = scalarTriple(pq, pa, pd)
      when(u >= 0f && w >= 0f):
        val negV = -v
        val denom = 1f / (u + negV + w)
        val uu = u * denom
        val vv = negV * denom
        val ww = w * denom
        val intersectPos = fixedQuad.a * uu + fixedQuad.d * vv + fixedQuad.c * ww
        checkHit(intersectPos)
      .otherwise:
        currentHit

  val sphere = Sphere(center = (0f, 1.5f, 2f), radius = 0.5f, color = (1f, 1f, 1f), emissive = (30f, 30f, 30f))

  val sphereRed = Sphere(center = (0f, 0f, 4f), radius = 0.5f, color = (1f, 1f, 1f), emissive = (0f, 0f, 0f))

  val sphereGreen = Sphere(center = (1.5f, 0f, 4f), radius = 0.5f, color = (0f, 1f, 0f), emissive = (0f, 0f, 0f))

  val sphereBlue = Sphere(center = (-1.5f, 0f, 4f), radius = 0.5f, color = (0f, 0f, 1f), emissive = (0f, 0f, 5f))

  val backWall = Quad(a = (-5f, -5f, 5f), b = (5f, -5f, 5f), c = (5f, 5f, 5f), d = (-5f, 5f, 5f), color = (1f, 1f, 1f), emissive = (0f, 0f, 0f))

  def getColorForRay(rayPos: Vec3[Float32], rayDirection: Vec3[Float32], rngState: UInt32): RayTraceState =
    val noHitState = RayTraceState(rayPos = rayPos, rayDir = rayDirection, color = (0f, 0f, 0f), throughput = (1f, 1f, 1f), rngSeed = rngState)
    GSeq
      .gen[RayTraceState](
        first = noHitState,
        next = { case state @ RayTraceState(rayPos, rayDir, color, throughput, rngSeed, _) =>
          val noHit = RayHitInfo(1000f, (0f, 0f, 0f), (0f, 0f, 0f), (0f, 0f, 0f))
          val sphereHit = testSphereTrace(rayPos, rayDir, noHit, sphere)
          val sphereRedHit = testSphereTrace(rayPos, rayDir, sphereHit, sphereRed)
          val sphereGreenHit = testSphereTrace(rayPos, rayDir, sphereRedHit, sphereGreen)
          val sphereBlueHit = testSphereTrace(rayPos, rayDir, sphereGreenHit, sphereBlue)
          val wallHit = testQuadTrace(rayPos, rayDir, sphereBlueHit, backWall)
          val Random(rndVec, nextSeed) = randomVector(rngSeed)
          val diffuseRayDir = normalize(wallHit.normal + rndVec)
          RayTraceState(
            rayPos = rayPos + rayDir * wallHit.dist + wallHit.normal * rayPosNudge,
            rayDir = diffuseRayDir,
            color = color + wallHit.emissive mulV throughput,
            throughput = throughput mulV wallHit.albedo,
            finished = wallHit.dist > superFar,
            rngSeed = nextSeed,
          )
        },
      )
      .limit(maxBounces)
      .takeWhile(!_.finished)
      .lastOr(noHitState)

  case class RenderIteration(color: Vec3[Float32], rngState: UInt32) extends GStruct[RenderIteration]

  val raytracing: GFunction[Empty, Vec4[Float32], Vec4[Float32]] = GFunction.from2D(dim):
    case (_, (xi: Int32, yi: Int32), _) =>
      val rngState = xi * 1973 + yi * 9277 + 2137 * 26699 | 1
      val color = GSeq
        .gen(
          first = RenderIteration((0f, 0f, 0f), rngState.unsigned),
          next = { case RenderIteration(_, rngState) =>
            val Random(wiggleX, rngState1) = randomFloat(rngState)
            val Random(wiggleY, rngState2) = randomFloat(rngState1)
            val x = ((xi.asFloat + wiggleX) / dim.toFloat) * 2f - 1f
            val y = ((yi.asFloat + wiggleY) / dim.toFloat) * 2f - 1f
            val rayPosition = (0f, 0f, 0f)
            val cameraDist = 1.0f / tan(fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
            val rayTarget = (x, y, cameraDist)
            val rayDir = normalize(rayTarget - rayPosition)
            val rtResult = getColorForRay(rayPosition, rayDir, rngState2)
            RenderIteration(rtResult.color, rtResult.rngSeed)
          },
        )
        .limit(pixelIterationsPerFrame)
        .fold((0f, 0f, 0f), { case (acc, RenderIteration(color, _)) => acc + (color * (1.0f / pixelIterationsPerFrame.toFloat)) })
      (color, 1f)

  val mem = Vec4FloatMem(Array.fill(dim * dim)((0f, 0f, 0f, 0f)))
  val result = mem.map(raytracing).asInstanceOf[Vec4FloatMem].toArray
  ImageUtility.renderToImage(result, dim, Paths.get(s"generated4.png"))
