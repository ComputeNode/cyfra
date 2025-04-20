package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.dsl.{GStruct, given}
import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Control.when
import io.computenode.cyfra.dsl.Value.*

case class Plane(
  point: Vec3[Float32],
  normal: Vec3[Float32],
  material: Material
) extends GStruct[Plane] with Shape:
  def testRay(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    currentHit: RayHitInfo,
  ): RayHitInfo =
    val denom = normal dot rayDir
    given epsilon: Float32 = 0.1f
    when(denom =~= 0.0f) {
      currentHit
    } otherwise {
      val t = ((point - rayPos) dot normal) / denom
      when(t < 0.0f || t >= currentHit.dist) {
        currentHit
      } otherwise {
        val hitNormal = when(denom < 0.0f)(normal).otherwise(-normal)
        RayHitInfo(t, hitNormal, material)
      }
    }
