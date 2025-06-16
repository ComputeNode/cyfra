package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.dsl.library.Functions.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.rt.shapes.Shape.TestRay
import io.computenode.cyfra.dsl.control.Pure.pure
import io.computenode.cyfra.dsl.struct.GStruct

case class Plane(point: Vec3[Float32], normal: Vec3[Float32], material: Material) extends GStruct[Plane] with Shape

object Plane:
  given TestRay[Plane] with
    def testRay(plane: Plane, rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo): RayHitInfo = pure:
      val denom = plane.normal dot rayDir
      given epsilon: Float32 = 0.1f
      when(denom =~= 0.0f) {
        currentHit
      } otherwise {
        val t = ((plane.point - rayPos) dot plane.normal) / denom
        when(t < 0.0f || t >= currentHit.dist) {
          currentHit
        } otherwise {
          val hitNormal = when(denom < 0.0f)(plane.normal).otherwise(-plane.normal)
          RayHitInfo(t, hitNormal, plane.material)
        }
      }
