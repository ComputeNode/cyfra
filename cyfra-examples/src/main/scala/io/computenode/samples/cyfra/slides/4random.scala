package io.computenode.samples.cyfra.slides

import java.nio.file.Paths
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.dsl.GStruct.Empty
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.utility.ImageUtility
import izumi.reflect.macrortti.LightTypeTag


@main
def randomRays =
  val raysPerPixel = 10
  val dim = 1024
  val fovDeg = 80
  val minRayHitTime = 0.01f
  val superFar = 999f
  val maxBounces = 100
  val rayPosNudge = 0.001f
  val pixelIterationsPerFrame = 3000

  def scalarTriple(u: Vec3[Float32], v: Vec3[Float32], w: Vec3[Float32]): Float32 = (u cross v) dot w

  case class Sphere(
    center: Vec3[Float32],
    radius: Float32,
    color: Vec3[Float32],
    emissive: Vec3[Float32],
    specular: Float32 = 0.2f
  ) extends GStruct[Sphere]


  case class Quad(
    a: Vec3[Float32],
    b: Vec3[Float32],
    c: Vec3[Float32],
    d: Vec3[Float32],
    color: Vec3[Float32],
    emissive: Vec3[Float32],
    specular: Float32 = 0.01f
  ) extends GStruct[Quad]

  case class RayHitInfo(
    dist: Float32,
    normal: Vec3[Float32],
    albedo: Vec3[Float32],
    emissive: Vec3[Float32],
    specular: Float32
  ) extends GStruct[RayHitInfo]

  case class RayTraceState(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    color: Vec3[Float32],
    throughput: Vec3[Float32],
    rngSeed: UInt32,
    finished: GBoolean = false
  ) extends GStruct[RayTraceState]


  def testSphereTrace(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    currentHit: RayHitInfo,
    sphere: Sphere
  ): RayHitInfo =
    val toRay = rayPos - sphere.center
    val b = toRay dot rayDir
    val c = (toRay dot toRay) - (sphere.radius * sphere.radius)
    val notHit = currentHit
    when(c > 0f && b > 0f) {
      notHit
    } otherwise {
      val discr = b * b - c
      when(discr > 0f) {
        val initDist = -b - sqrt(discr)
        val fromInside = initDist < 0f
        val dist = when(fromInside)(-b + sqrt(discr)).otherwise(initDist)
        when(dist > minRayHitTime && dist < currentHit.dist) {
          val normal = normalize(rayPos + rayDir * dist - sphere.center)
          RayHitInfo(dist, normal, sphere.color, sphere.emissive, sphere.specular)
        } otherwise {
          notHit
        }
      } otherwise {
        notHit
      }
    }

  def testQuadTrace(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    currentHit: RayHitInfo,
    quad: Quad
  ): RayHitInfo =
    val normal = normalize((quad.c - quad.a) cross (quad.c - quad.b))
    val fixedQuad = when((normal dot rayDir) > 0f) {
      Quad(quad.d, quad.c, quad.b, quad.a, quad.color, quad.emissive, quad.specular)
    } otherwise {
      quad
    }
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
      }.otherwise {
        (intersectPoint.z - rayPos.z) / rayDir.z
      }
      when(dist > minRayHitTime && dist < currentHit.dist) {
        RayHitInfo(dist, fixedNormal, quad.color, quad.emissive, quad.specular)
      } otherwise {
        currentHit
      }

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
      } otherwise {
        currentHit
      }
    } otherwise {
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
      } otherwise {
        currentHit
      }
    }

  val sphereBlue = Sphere(
    center = (-1.5f, 0f, 4f),
    radius = 0.5f,
    color = (0.5f, 0.5f, 1f),
    emissive = (0f, 0f, 0f),
    specular = 0.6f
  )

  val sphereRed = Sphere(
    center = (0f, 0f, 4f),
    radius = 0.5f,
    color = (1f, 0.5f, 0.5f),
    emissive = (0f, 0f, 0f),
    specular = 0.2f
  )

  val sphereGreen = Sphere(
    center = (1.5f, 0f, 4f),
    radius = 0.5f,
    color = (0f, 1f, 0f),
    emissive = (0.5f, 0.5f, 0f),
    specular = 0.08f
  )



  val backWall = Quad(
    a = (-3f, -3f, 5f),
    b = (3f, -3f, 5f),
    c = (3f, 3f, 5f),
    d = (-3f, 3f, 5f),
    color = (1f, 1f, 1f),
    emissive = (0f, 0f, 0f),
  )

  val leftWall = Quad(
    a = (-3f, -3f, 5f),
    b = (-3f, -3f, -3f),
    c = (-3f, 3f, -3f),
    d = (-3f, 3f, 5f),
    color = (1f, 0.5f, 0.5f),
    emissive = (0f, 0f, 0f),
  )

  val rightWall = Quad(
    a = (3f, -3f, 5f),
    b = (3f, -3f, -3f),
    c = (3f, 3f, -3f),
    d = (3f, 3f, 5f),
    color = (0.5f, 1f, 0.5f),
    emissive = (0f, 0f, 0f),
  )

  val ceiling = Quad(
    a = (-3f, -3f, -3f),
    b = (3f, -3f, -3f),
    c = (3f, -3f, 5f),
    d = (-3f, -3f, 5f),
    color = (0.5f, 0.5f, 0.5f),
    emissive = (0f, 0f, 0f),
  )

  val floor = Quad(
    a = (-3f, 3f, -3f),
    b = (3f, 3f, -3f),
    c = (3f, 3f, 5f),
    d = (-3f, 3f, 5f),
    color = (0.5f, 0.5f, 0.5f),
    emissive = (0f, 0f, 0f),
  )

  val ceilingLamp = Quad(
    a = (-1f, -2.9f, 2f),
    b = (1f, -2.9f, 2f),
    c = (1f, -2.9f, 4f),
    d = (-1f, -2.9f, 4f),
    color = (1f, 1f, 1f),
    emissive = (1f, 1f, 1f) * 32f,
  )

  val walls = List(
    backWall,
    leftWall,
    rightWall,
    floor,
    ceiling,
    ceilingLamp
  )

  val spheres = List(
    sphereRed,
    sphereGreen,
    sphereBlue
  )

  val NoHit = RayHitInfo(1000f, (0f, 0f, 0f), (0f, 0f, 0f), (0f, 0f, 0f), 0f)

  def testScene(rayPos: Vec3[Float32], rayDir: Vec3[Float32]): RayHitInfo =
    val spheresHit = GSeq.of(spheres).fold(NoHit, (currentHit, sphere) =>
      testSphereTrace(rayPos, rayDir, currentHit, sphere)
    )
    GSeq.of(walls).fold(spheresHit, (currentHit, wall) =>
      testQuadTrace(rayPos, rayDir, currentHit, wall)
    )

  def bounce(rayTraceState: RayTraceState): RayTraceState =
    val random = Random(rayTraceState.rngSeed)
    val RayTraceState(rayPos, rayDir, color, throughput, rngSeed, _) = rayTraceState
    val sceneHit = testScene(rayPos, rayDir)

    val (random1, rndVec) = random.next[Vec3[Float32]]
    val diffuseRayDir = normalize(sceneHit.normal + rndVec)
    val specularRayDir = reflect(rayDir, sceneHit.normal)
    val (random2, specularDiceRoll) = random1.next[Float32]
    val reflectedRayDir = when(specularDiceRoll < sceneHit.specular):
      specularRayDir
    .otherwise:
      diffuseRayDir

    RayTraceState(
      rayPos = rayPos + rayDir * sceneHit.dist + sceneHit.normal * rayPosNudge,
      rayDir = reflectedRayDir,
      color = color + sceneHit.emissive mulV throughput,
      throughput = throughput mulV sceneHit.albedo,
      finished = sceneHit.dist > superFar,
      rngSeed = random2.seed
    )

  def getColorForRay(ray: Ray, rngState: UInt32): RayTraceState =
    val noHitState = RayTraceState(
      rayPos = ray.position,
      rayDir = ray.direction,
      color = (0f, 0f, 0f),
      throughput = (1f, 1f, 1f),
      rngSeed = rngState,
    )
    GSeq.gen[RayTraceState](
      first = noHitState,
      next = bounce
    )
      .limit(maxBounces)
      .takeWhile(!_.finished)
      .lastOr(noHitState)

  case class RenderIteration(color: Vec3[Float32], rngState: UInt32) extends GStruct[RenderIteration]

  case class Ray(
    position: Vec3[Float32],
    direction: Vec3[Float32],
  ) extends GStruct[Ray]

  def rayForPixel(xi: Int32, yi: Int32, random: Random): Ray =
    val (random1, wiggleX) = random.next[Float32]
    val (random2, wiggleY) = random.next[Float32]
    val x = ((xi.asFloat + wiggleX) / dim.toFloat) * 2f - 1f
    val y = ((yi.asFloat + wiggleY) / dim.toFloat) * 2f - 1f

    val rayPosition = (0f, 0f, -0.5f)
    val cameraDist = 1.0f / tan(fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
    val rayTarget = (x, y, cameraDist)
    val rayDir = normalize(rayTarget - rayPosition)

    Ray(rayPosition, rayDir)

  val raytracing: GFunction[Empty, Vec4[Float32], Vec4[Float32]] = GFunction.from2D(dim):
    case (_, (xi: Int32, yi: Int32), _) =>
      val rngState = xi * 1973 + yi * 9277 + 2137 * 26699 | 1
      val startState = RenderIteration((0f, 0f, 0f), rngState.unsigned)
      val color = GSeq.gen(first = startState, next = {
          case RenderIteration(_, rngState) =>
            val ray = rayForPixel(xi, yi, Random(rngState))
            val rtResult = getColorForRay(ray, rngState + 1)
            RenderIteration(rtResult.color, rtResult.rngSeed)
        }).limit(pixelIterationsPerFrame)
        .map(state => state.color * (1.0f / pixelIterationsPerFrame.toFloat))
        .fold((0f, 0f, 0f), _ + _)
      (color, 1f)

  val mem = Vec4FloatMem(Array.fill(dim * dim)((0f,0f,0f,0f)))
  val result = mem.map(raytracing).asInstanceOf[Vec4FloatMem].toArray
  ImageUtility.renderToImage(result, dim, Paths.get(s"generated4.png"))

