package io.computenode.cyfra.fluids.visualization

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct

/** Camera parameters for 3D rendering.
  *
  * Uses Vec4 instead of Vec3 for proper 16-byte alignment in GPU memory.
  * The w component is unused for position/target/up vectors.
  *
  * @param position Camera position in world space (w unused)
  * @param target Point the camera is looking at (w unused)
  * @param up Up vector (typically (0, 1, 0, 0)) (w unused)
  * @param fov Field of view in degrees
  * @param aspectRatio Width / height ratio
  */
case class Camera3D(
  position: Vec4[Float32],
  target: Vec4[Float32],
  up: Vec4[Float32],
  fov: Float32,
  aspectRatio: Float32
) extends GStruct[Camera3D]

object Camera3D:
  /** Create a default camera looking at the origin from (0, 0, 5) */
  def default(aspectRatio: Float): Camera3D = Camera3D(
    position = (0.0f, 0.0f, 5.0f, 0.0f),  // Vec4, w unused
    target = (0.0f, 0.0f, 0.0f, 0.0f),
    up = (0.0f, 1.0f, 0.0f, 0.0f),
    fov = 45.0f,
    aspectRatio = aspectRatio
  )
  
  /** Create a camera orbiting around a center point */
  def orbit(
    centerX: Float,
    centerY: Float,
    centerZ: Float,
    radius: Float,
    angle: Float,
    height: Float,
    aspectRatio: Float
  ): Camera3D =
    import scala.math.{cos, sin}
    // Compute ALL values on host side before creating tuple
    val x = centerX + radius * cos(angle).toFloat
    val y = centerY + height
    val z = centerZ + radius * sin(angle).toFloat
    Camera3D(
      position = (x, y, z, 0.0f),  // Vec4 with w=0
      target = (centerX, centerY, centerZ, 0.0f),
      up = (0.0f, 1.0f, 0.0f, 0.0f),
      fov = 45.0f,
      aspectRatio = aspectRatio
    )
