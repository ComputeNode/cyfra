package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/** 
 * Vector3 convenience utilities.
 * 
 * Note: The DSL already provides: `length`, `normalize`, `dot`, `+`, `*`.
 * This object only adds shortcuts not in the core DSL.
 */
object Vec3Ops:

  /** Construct Vec3[Float32] - convenience alias for vec3. */
  def vec3f(x: Float32, y: Float32, z: Float32): Vec3[Float32] = vec3(x, y, z)

  /** Negate a vector (DSL doesn't have unary minus for Vec3). */
  def negate3(v: Vec3[Float32]): Vec3[Float32] = vec3(-v.x, -v.y, -v.z)
