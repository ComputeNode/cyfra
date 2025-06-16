package io.computenode.cyfra.foton.rt.shapes

import io.computenode.cyfra.foton.rt.shapes.*
import io.computenode.cyfra.foton.rt.Material
import io.computenode.cyfra.dsl.{GSeq, GStruct, given}
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import izumi.reflect.Tag
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.rt.shapes.Shape.TestRay

import scala.util.chaining.*

class ShapeCollection(val boxes: List[Box], val spheres: List[Sphere], val quads: List[Quad], val planes: List[Plane]) extends Shape:

  def this(shapes: List[Shape]) =
    this(
      shapes.collect { case box: Box => box },
      shapes.collect { case sphere: Sphere => sphere },
      shapes.collect { case quad: Quad => quad },
      shapes.collect { case plane: Plane => plane },
    )

  def addShape(shape: Shape): ShapeCollection =
    shape match
      case box: Box =>
        ShapeCollection(box :: boxes, spheres, quads, planes)
      case sphere: Sphere =>
        ShapeCollection(boxes, sphere :: spheres, quads, planes)
      case quad: Quad =>
        ShapeCollection(boxes, spheres, quad :: quads, planes)
      case plane: Plane =>
        ShapeCollection(boxes, spheres, quads, plane :: planes)
      case _ => assert(false, "Unknown shape type: Broken sealed hierarchy")

  def testRay(rayPos: Vec3[Float32], rayDir: Vec3[Float32], noHit: RayHitInfo): RayHitInfo =
    def testShapeType[T <: GStruct[T] & Shape: FromExpr: Tag: TestRay](shapes: List[T], currentHit: RayHitInfo): RayHitInfo =
      val testRay = summon[TestRay[T]]
      if shapes.isEmpty then currentHit
      else GSeq.of(shapes).fold(currentHit, (currentHit, shape) => testRay.testRay(shape, rayPos, rayDir, currentHit))

    testShapeType(quads, noHit)
      .pipe(testShapeType(spheres, _))
      .pipe(testShapeType(boxes, _))
      .pipe(testShapeType(planes, _))
