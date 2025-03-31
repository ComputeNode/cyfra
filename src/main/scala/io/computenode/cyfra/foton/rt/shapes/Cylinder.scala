package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.GStruct
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.{MinRayHitTime, RayHitInfo}

import java.nio.file.Paths
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
case class Cylinder(
  center: Vec3[Float32],
  radius: Float32,
  height: Float32,
  material: Material
) extends GStruct[Cylinder] with Shape:
  def testRay(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    currentHit: RayHitInfo,
  ): RayHitInfo =
    val capTop = center.y + height/2f
    val capBottom = center.y - height/2f

    val ox = rayPos.x - center.x
    val oz = rayPos.z - center.z

    val a = rayDir.x * rayDir.x + rayDir.z * rayDir.z
    val b = 2f * (ox * rayDir.x + oz * rayDir.z)
    val c = ox * ox + oz * oz - radius * radius
    val discr = b * b - 4f * a * c

    val sideHit = when(discr >= 0f && !(a === 0f)) {
      val sqrtD = sqrt(discr)
      val t0 = (-b - sqrtD) / (2f * a)
      val t1 = (-b + sqrtD) / (2f * a)
      val t = when(t0 > 0f)(t0).elseWhen(t1 > 0f)(t1).otherwise(-1f)
      when(t > 0f) {
        val hitY = rayPos.y + rayDir.y * t
        when(hitY >= capBottom && hitY <= capTop && t < currentHit.dist) {
          val hitPoint = rayPos + rayDir * t
          val normal = normalize((hitPoint.x - center.x, 0f, hitPoint.z - center.z))
          RayHitInfo(t, normal, material)
        } otherwise currentHit
      } otherwise currentHit
    } otherwise currentHit

    val bottomHit = when(!(rayDir.y === 0f)) {
      val t = (capBottom - rayPos.y) / rayDir.y
      when(t > 0f && t < currentHit.dist) {
        val hitPoint = rayPos + rayDir * t
        val dx = hitPoint.x - center.x
        val dz = hitPoint.z - center.z
        val dist2 = dx * dx + dz * dz
        when(dist2 <= radius * radius) {
          RayHitInfo(t, (0f, -1f, 0f), material)
        } otherwise currentHit
      } otherwise currentHit
    } otherwise currentHit

    val topHit = when(!(rayDir.y === 0f)) {
      val t = (capTop - rayPos.y) / rayDir.y
      when(t > 0f && t < currentHit.dist) {
        val hitPoint = rayPos + rayDir * t
        val dx = hitPoint.x - center.x
        val dz = hitPoint.z - center.z
        val dist2 = dx * dx + dz * dz
        when(dist2 <= radius * radius) {
          RayHitInfo(t, (0f, 1f, 0f), material)
        } otherwise currentHit
      } otherwise currentHit
    } otherwise currentHit

    val bestHit = 
      when(sideHit.dist <= bottomHit.dist && sideHit.dist <= topHit.dist){sideHit}
      .elseWhen(bottomHit.dist <= sideHit.dist && bottomHit.dist <= topHit.dist){bottomHit}
      .otherwise(topHit)

    when(bestHit.dist < currentHit.dist)(bestHit) otherwise currentHit
