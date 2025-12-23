package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.dsl.archive.library.Math3D.scalarTriple
import io.computenode.cyfra.foton.rt.RtRenderer.{MinRayHitTime, RayHitInfo}

import java.nio.file.Paths
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.foton.rt.shapes.Shape.TestRay
import io.computenode.cyfra.dsl.archive.control.Pure.pure
import io.computenode.cyfra.dsl.archive.struct.GStruct

case class Quad(a: Vec3[Float32], b: Vec3[Float32], c: Vec3[Float32], d: Vec3[Float32], material: Material) extends GStruct[Quad] with Shape

object Quad:
  given TestRay[Quad] with
    def testRay(quad: Quad, rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo): RayHitInfo = pure:
      val normal = normalize((quad.c - quad.a) cross (quad.c - quad.b))
      val fixedQuad = when((normal dot rayDir) > 0f):
        Quad(quad.d, quad.c, quad.b, quad.a, quad.material)
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
        when(dist > MinRayHitTime && dist < currentHit.dist):
          RayHitInfo(dist, fixedNormal, quad.material)
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
