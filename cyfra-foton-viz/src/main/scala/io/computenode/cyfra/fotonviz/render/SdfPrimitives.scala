package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/**
 * SDF primitive shapes.
 *
 * Each primitive has two versions:
 * - Raw version returning Float32 (distance only)
 * - Material version returning SdfResult (distance + material)
 */
object SdfPrimitives:

  // ==== Raw Primitives (Float32) ====

  /** Sphere centered at origin. */
  def sphere(p: Vec3[Float32], radius: Float32): Float32 =
    sqrt(p.x * p.x + p.y * p.y + p.z * p.z) - radius

  /** Sphere at arbitrary center. */
  def sphereAt(p: Vec3[Float32], center: Vec3[Float32], radius: Float32): Float32 =
    val dx = p.x - center.x
    val dy = p.y - center.y
    val dz = p.z - center.z
    sqrt(dx * dx + dy * dy + dz * dz) - radius

  /** Box centered at origin with given half-extents. */
  def box(p: Vec3[Float32], halfSize: Vec3[Float32]): Float32 =
    val qx = abs(p.x) - halfSize.x
    val qy = abs(p.y) - halfSize.y
    val qz = abs(p.z) - halfSize.z
    val outside = sqrt(max(qx, 0.0f) * max(qx, 0.0f) + max(qy, 0.0f) * max(qy, 0.0f) + max(qz, 0.0f) * max(qz, 0.0f))
    val inside = min(max(qx, max(qy, qz)), 0.0f)
    outside + inside

  /** Infinite plane at y = height. */
  def planeY(p: Vec3[Float32], height: Float32): Float32 =
    p.y - height

  /** Torus centered at origin, lying in XZ plane. */
  def torus(p: Vec3[Float32], majorRadius: Float32, minorRadius: Float32): Float32 =
    val qx = sqrt(p.x * p.x + p.z * p.z) - majorRadius
    sqrt(qx * qx + p.y * p.y) - minorRadius

  /** Cylinder along Y axis. */
  def cylinderY(p: Vec3[Float32], radius: Float32, halfHeight: Float32): Float32 =
    val d = sqrt(p.x * p.x + p.z * p.z) - radius
    val h = abs(p.y) - halfHeight
    min(max(d, h), 0.0f) + sqrt(max(d, 0.0f) * max(d, 0.0f) + max(h, 0.0f) * max(h, 0.0f))

  // ==== Primitives with Material (SdfResult) ====

  /** Sphere with material. */
  def sphereMat(p: Vec3[Float32], radius: Float32, materialId: Int32): SdfResult =
    SdfResult(sphere(p, radius), materialId)

  /** Sphere at center with material. */
  def sphereAtMat(p: Vec3[Float32], center: Vec3[Float32], radius: Float32, materialId: Int32): SdfResult =
    SdfResult(sphereAt(p, center, radius), materialId)

  /** Box with material. */
  def boxMat(p: Vec3[Float32], halfSize: Vec3[Float32], materialId: Int32): SdfResult =
    SdfResult(box(p, halfSize), materialId)

  /** Plane with material. */
  def planeYMat(p: Vec3[Float32], height: Float32, materialId: Int32): SdfResult =
    SdfResult(planeY(p, height), materialId)

  /** Torus with material. */
  def torusMat(p: Vec3[Float32], majorRadius: Float32, minorRadius: Float32, materialId: Int32): SdfResult =
    SdfResult(torus(p, majorRadius, minorRadius), materialId)

  /** Cylinder with material. */
  def cylinderYMat(p: Vec3[Float32], radius: Float32, halfHeight: Float32, materialId: Int32): SdfResult =
    SdfResult(cylinderY(p, radius, halfHeight), materialId)

  // ==== Convenience: Wrap distance with material ====

  /** Wrap any distance with a material ID. */
  def withMaterial(distance: Float32, materialId: Int32): SdfResult =
    SdfResult(distance, materialId)
