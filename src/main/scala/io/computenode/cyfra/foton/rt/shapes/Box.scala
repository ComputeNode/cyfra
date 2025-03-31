package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.GStruct
import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo

case class Box(
  minV: Vec3[Float32],
  maxV: Vec3[Float32],
  material: Material
) extends GStruct[Box] with Shape:
  def testRay(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    currentHit: RayHitInfo,
  ): RayHitInfo =
    val tx1 = (minV.x - rayPos.x) / rayDir.x
    val tx2 = (maxV.x - rayPos.x) / rayDir.x
    val tMinX = min(tx1, tx2)
    val tMaxX = max(tx1, tx2)
    
    val ty1 = (minV.y - rayPos.y) / rayDir.y
    val ty2 = (maxV.y - rayPos.y) / rayDir.y
    val tMinY = min(ty1, ty2)
    val tMaxY = max(ty1, ty2)
    
    val tz1 = (minV.z - rayPos.z) / rayDir.z
    val tz2 = (maxV.z - rayPos.z) / rayDir.z
    val tMinZ = min(tz1, tz2)
    val tMaxZ = max(tz1, tz2)
    
    val tEnter = max(tMinX, tMinY, tMinZ)
    val tExit = min(tMaxX, tMaxY, tMaxZ)
    
    when(tEnter > tExit || tExit < 0.0f) {
      currentHit
    } otherwise {
      val hitDistance = when(tEnter > 0f)(tEnter).otherwise(tExit)
      val hitNormal = when(tEnter =~= tMinX) {
        (when(rayDir.x > 0f)(-1f).otherwise(1f), 0f, 0f)
      }.elseWhen(tEnter =~= tMinY) {
        (0f, when(rayDir.y > 0f)(-1f).otherwise(1f), 0f)
      }.otherwise {
        (0f, 0f, when(rayDir.z > 0f)(-1f).otherwise(1f))
      }
      RayHitInfo(hitDistance, hitNormal, material)
    }
      
      
      