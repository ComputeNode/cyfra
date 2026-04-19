package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.foton.rt.shapes.Shape.TestRay
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.library.*
import io.computenode.cyfra.foton.library.given
import io.computenode.cyfra.foton.given
import io.computenode.cyfra.foton.library.pure

case class Box(minV: Vec3[Float32], maxV: Vec3[Float32], material: Material) extends Shape

object Box:
  given TestRay[Box] with
    val testRay = pure:
      case (box, rayPos, rayDir, currentHit) =>
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

        when(tEnter < tExit || tExit < 0.0f):
          currentHit
        .otherwise:
          val hitDistance = when(tEnter > 0f)(tEnter).otherwise(tExit)
          val hitNormal: Vec3[Float32] = when(tEnter =~= tMinX):
            Vec3(when[Float32](rayDir.x > 0f)(-1f).otherwise(1f), 0f, 0f)
          .elseWhen(tEnter =~= tMinY):
            Vec3(0f, when[Float32](rayDir.y > 0f)(-1f).otherwise(1f), 0f)
          .otherwise:
            Vec3(0f, 0f, when[Float32](rayDir.z > 0f)(-1f).otherwise(1f))
          RayHitInfo(hitDistance, hitNormal, box.material)
