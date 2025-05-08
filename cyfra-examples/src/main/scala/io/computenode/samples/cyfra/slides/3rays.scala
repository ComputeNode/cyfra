package io.computenode.samples.cyfra.slides

import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Algebra.given
import io.computenode.cyfra.dsl.Value.{Float32, Int32, Vec4}
import io.computenode.cyfra.dsl.given

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Paths
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.compiletime.error
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.given
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Control.when
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.GStruct.Empty
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.utility.ImageUtility

@main
def rays =
  val raysPerPixel = 10
  val dim = 1024
  val fovDeg = 60
  val minRayHitTime = 0.01f
  val superFar = 999f
  val maxBounces = 10
  val rayPosNudge = 0.001f
  
  def scalarTriple(u: Vec3[Float32], v: Vec3[Float32], w: Vec3[Float32]): Float32 = (u cross v) dot w

  case class Sphere(
    center: Vec3[Float32],
    radius: Float32,
    color: Vec3[Float32],
    emissive: Vec3[Float32],
  ) extends GStruct[Sphere]


  case class Quad(
    a: Vec3[Float32],
    b: Vec3[Float32],
    c: Vec3[Float32],
    d: Vec3[Float32],
    color: Vec3[Float32],
    emissive: Vec3[Float32],
  ) extends GStruct[Quad]

  case class RayHitInfo(
    dist: Float32,
    normal: Vec3[Float32],
    albedo: Vec3[Float32],
    emissive: Vec3[Float32],
  ) extends GStruct[RayHitInfo]

  case class RayTraceState(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    color: Vec3[Float32],
    throughput: Vec3[Float32],
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
          RayHitInfo(dist, normal, sphere.color, sphere.emissive)
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
      Quad(quad.d, quad.c, quad.b, quad.a, quad.color, quad.emissive)
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
        RayHitInfo(dist, fixedNormal, quad.color, quad.emissive)
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

  val sphere = Sphere(
    center = (1.5f, 1.5f, 4f),
    radius = 0.5f,
    color = (1f, 1f, 1f),
    emissive = (3f, 3f, 3f)
  )

  val backWall = Quad(
    a = (-2f, -2f, 5f),
    b = (2f, -2f, 5f),
    c = (2f, 2f, 5f),
    d = (-2f, 2f, 5f),
    color = (0f, 1f, 1f),
    emissive = (0f, 0f, 0f)
  )

  def getColorForRay(rayPos: Vec3[Float32], rayDirection: Vec3[Float32]): Vec4[Float32] =
    GSeq.gen[RayTraceState](
      first = RayTraceState(
        rayPos = rayPos,
        rayDir = rayDirection,
        color = (0f, 0f, 0f),
        throughput = (1f, 1f, 1f)
      ),
      next = {
        case state @ RayTraceState(rayPos, rayDir, color, throughput, _) =>
          val noHit = RayHitInfo(1000f, (0f, 0f, 0f), (0f, 0f, 0f), (0f, 0f, 0f))
          val sphereHit = testSphereTrace(rayPos, rayDir, noHit, sphere)
          val wallHit = testQuadTrace(rayPos, rayDir, sphereHit, backWall)
          RayTraceState(
            rayPos = rayPos + rayDir * wallHit.dist + wallHit.normal * rayPosNudge,
            rayDir = reflect(rayDir, wallHit.normal),
            color = color + wallHit.emissive mulV throughput,
            throughput = throughput mulV wallHit.albedo,
            finished = wallHit.dist > superFar
          )
      })
      .limit(maxBounces)
      .takeWhile(!_.finished)
      .map(state => (state.color, 1f))
      .lastOr((0f,0f,0f,1f))

  val raytracing: GFunction[Empty, Vec4[Float32], Vec4[Float32]] = GFunction.from2D(dim, {
    case (_, (xi: Int32, yi: Int32), _) =>
      val x = (xi.asFloat / dim.toFloat) * 2f - 1f
      val y = (yi.asFloat / dim.toFloat) * 2f - 1f

      val rayPosition = (0f, 0f, 0f)
      val cameraDist = 1.0f / tan(fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
      val rayTarget = (x, y, cameraDist)

      val rayDir = normalize(rayTarget - rayPosition)
      getColorForRay(rayPosition, rayDir)
  })


  val mem = Vec4FloatMem(Array.fill(dim * dim)((0f,0f,0f,0f)))
  val result = mem.map(raytracing).asInstanceOf[Vec4FloatMem].toArray
  ImageUtility.renderToImage(result, dim, Paths.get(s"generated3.png"))
