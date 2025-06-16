package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.foton.rt.shapes.Shape.TestRay
import io.computenode.cyfra.dsl.control.Pure.pure
import io.computenode.cyfra.dsl.struct.GStruct

case class Box(minV: Vec3[Float32], maxV: Vec3[Float32], material: Material) extends GStruct[Box] with Shape

object Box:
  given TestRay[Box] with
    def testRay(box: Box, rayPos: Vec3[Float32], rayDir: Vec3[Float32], currentHit: RayHitInfo): RayHitInfo = pure:
      val tx1 = (box.minV.x - rayPos.x) / rayDir.x
      val tx2 = (box.maxV.x - rayPos.x) / rayDir.x
      val tMinX = min(tx1, tx2)
      val tMaxX = max(tx1, tx2)

      val ty1 = (box.minV.y - rayPos.y) / rayDir.y
      val ty2 = (box.maxV.y - rayPos.y) / rayDir.y
      val tMinY = min(ty1, ty2)
      val tMaxY = max(ty1, ty2)

      val tz1 = (box.minV.z - rayPos.z) / rayDir.z
      val tz2 = (box.maxV.z - rayPos.z) / rayDir.z
      val tMinZ = min(tz1, tz2)
      val tMaxZ = max(tz1, tz2)

      val tEnter = max(tMinX, tMinY, tMinZ)
      val tExit = min(tMaxX, tMaxY, tMaxZ)

      when(tEnter < tExit || tExit < 0.0f) {
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
        RayHitInfo(hitDistance, hitNormal, box.material)
      }
