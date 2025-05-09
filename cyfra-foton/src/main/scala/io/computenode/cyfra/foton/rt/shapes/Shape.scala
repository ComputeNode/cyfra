package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*

trait Shape

object Shape:
  trait TestRay[S <: Shape]:
    def testRay(
      shape: S,
      rayPos: Vec3[Float32],
      rayDir: Vec3[Float32],
      currentHit: RayHitInfo,
    ): RayHitInfo
