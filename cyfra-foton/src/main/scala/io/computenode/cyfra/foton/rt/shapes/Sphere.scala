package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.control.Pure.pure
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.{MinRayHitTime, RayHitInfo}
import io.computenode.cyfra.foton.rt.shapes.Shape.TestRay

case class Sphere(center: Vec3[Float32], radius: Float32, material: Material) extends GStruct[Sphere] with Shape

object Sphere:
  given TestRay[Sphere] with
    def testRay(sphere: Sphere, rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo): RayHitInfo = pure:
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
          when(dist > MinRayHitTime && dist < currentHit.dist) {
            val normal = normalize((rayPos + rayDir * dist - sphere.center) * when(fromInside)(-1f).otherwise(1f))
            RayHitInfo(dist, normal, sphere.material, fromInside)
          } otherwise notHit
        } otherwise notHit
