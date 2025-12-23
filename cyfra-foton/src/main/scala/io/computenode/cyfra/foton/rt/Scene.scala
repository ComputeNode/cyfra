package io.computenode.cyfra.foton.rt

import io.computenode.cyfra.dsl.archive.Value.{Float32, Vec3}
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.foton.rt.shapes.{Shape, ShapeCollection}
import io.computenode.cyfra.given

import scala.util.chaining.*

case class Scene(shapes: List[Shape], camera: Camera):

  private val shapesCollection: ShapeCollection = ShapeCollection(shapes)

  def rayTest(rayPos: Vec3[Float32], rayDir: Vec3[Float32], noHit: RayHitInfo): RayHitInfo =
    shapesCollection.testRay(rayPos, rayDir, noHit)
