package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.foton.library.pure
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo

trait Shape

object Shape:
  trait TestRay[S <: Shape]:
    val testRay: pure[(S, Vec3[Float32], Vec3[Float32], RayHitInfo), RayHitInfo]
