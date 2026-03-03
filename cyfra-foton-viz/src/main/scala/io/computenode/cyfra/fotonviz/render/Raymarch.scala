package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.collections.GSeq

/** Raymarching (sphere tracing) utilities. */
object Raymarch:

  /**
   * March a ray through an SDF scene.
   *
   * @param rayOrigin Starting point of the ray
   * @param rayDir    Normalized ray direction
   * @param sdf       Scene SDF function
   * @param maxSteps  Maximum iteration count
   * @param maxDist   Maximum ray travel distance
   * @param hitThreshold Distance considered a surface hit
   * @param stepScale  Step size multiplier (< 1.0 for non-true SDFs)
   * @return Vec2(t, hitType) where t = distance traveled, hitType > 0.5 = hit
   */
  def march(
    rayOrigin: Vec3[Float32],
    rayDir: Vec3[Float32],
    sdf: Vec3[Float32] => Float32,
    maxSteps: Int = 256,
    maxDist: Float32 = 40.0f,
    hitThreshold: Float32 = 0.001f,
    stepScale: Float32 = 0.7f,
  ): Vec2[Float32] =
    GSeq.gen[Int32](0, _ + 1).limit(maxSteps)
      .fold(vec2(0.0f, 0.0f), (state: Vec2[Float32], _: Int32) =>
        val t = state.x
        val hitType = state.y
        when(hitType > 0.5f || t > maxDist)(state).otherwise:
          val p = vec3(
            rayOrigin.x + rayDir.x * t,
            rayOrigin.y + rayDir.y * t,
            rayOrigin.z + rayDir.z * t,
          )
          val d = sdf(p)
          val newT = t + max(d * stepScale, 0.001f)
          when(d < hitThreshold)(vec2(t, 1.0f)).otherwise(vec2(newT, 0.0f))
      )

  /**
   * Calculate surface normal via central differences.
   *
   * @param p   Surface point
   * @param sdf Scene SDF function
   * @param eps Epsilon for finite differences
   * @return Normalized surface normal
   */
  def normal(
    p: Vec3[Float32],
    sdf: Vec3[Float32] => Float32,
    eps: Float32 = 0.002f,
  ): Vec3[Float32] =
    val nx = sdf(vec3(p.x + eps, p.y, p.z)) - sdf(vec3(p.x - eps, p.y, p.z))
    val ny = sdf(vec3(p.x, p.y + eps, p.z)) - sdf(vec3(p.x, p.y - eps, p.z))
    val nz = sdf(vec3(p.x, p.y, p.z + eps)) - sdf(vec3(p.x, p.y, p.z - eps))
    val len = sqrt(nx * nx + ny * ny + nz * nz) + 0.0001f
    vec3(nx / len, ny / len, nz / len)

  /** Compute hit position from ray origin, direction, and distance. */
  def hitPoint(rayOrigin: Vec3[Float32], rayDir: Vec3[Float32], t: Float32): Vec3[Float32] =
    vec3(rayOrigin.x + rayDir.x * t, rayOrigin.y + rayDir.y * t, rayOrigin.z + rayDir.z * t)
