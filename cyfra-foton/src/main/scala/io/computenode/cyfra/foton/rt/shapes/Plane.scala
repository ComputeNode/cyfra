package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.library.*
import io.computenode.cyfra.foton.rt.shapes.Shape.TestRay

case class Plane(point: Vec3[Float32], normal: Vec3[Float32], material: Material) extends Shape

object Plane:
  given TestRay[Plane] with
    val testRay = pure:
      case (plane, rayPos, rayDir, currentHit) =>
        val denom = plane.normal dot rayDir
        given Epsilon = Epsilon(0.1f)
        when(denom =~= 0.0f):
          currentHit
        .otherwise:
          val t = ((plane.point - rayPos) dot plane.normal) / denom
          when(t < 0.0f || t >= currentHit.dist):
            currentHit
          .otherwise:
            val hitNormal = when(denom < 0.0f)(plane.normal)
              .otherwise(-plane.normal)
            RayHitInfo(t, hitNormal, plane.material)
