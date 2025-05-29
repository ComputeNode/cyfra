package io.computenode.samples.cyfra.foton

import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.animation.AnimationFunctions.{AnimationInstant, smooth}

import java.nio.file.Paths

object Sdf:

  val width = 1920
  val height = 1080
  val fov = 80f
  val stepSize = 0.01f
  val far = 20f

  @main
  def main =
    val f = AnimatedFunction.fromCoord(
      sdf,
      duration = Milliseconds(500)
    )

    val renderer = AnimatedFunctionRenderer(
      AnimatedFunctionRenderer.Parameters(
        width = width,
        height = height,
        framesPerSecond = 22
      )
    )
    renderer.renderFramesToDir(
      f,
      destinationPath = Paths.get("sdf")
    )


  case class Sdf(fn: Vec3[Float32] => AnimationInstant ?=> Float32)
  
  
  object Sdf:
    def sphere(center: Vec3[Float32], radius: Float32): Sdf =
      Sdf(uv => length(uv - center) - radius)

  val shape = Sdf.sphere(center = vec3(0f, 0f, 10f), radius = 1f)
  
  def sdf(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =

    val aspectRatio: Float = width / height.toFloat
    val x = uv.x
    val y = uv.y / aspectRatio

    val rayPosition = vec3(0f, 0f, 0f)
    val cameraDist = 1.0f / tan(fov * 0.6f * math.Pi.toFloat / 180.0f)
    val rayTarget = (x, y, cameraDist)

    val rayDir = normalize(rayTarget - rayPosition)
    
    val dist = GSeq.gen[Int32](0, next = _ + 1).map: i =>
      val delta = rayDir * stepSize * i.asFloat
      val pos = rayPosition + delta
      val dist = shape.fn(pos)
      dist
//    .limit((far / stepSize).toInt)
//    .takeWhile(_ > 0f)
//    .filter(dist => dist > 0f)
//    .firstOr(1f)

    when(1f < 0f):
      (1.0f, 1.0f, 1.0f, 1.0f)
    .otherwise:
      (0.0f, 0.0f, 0.0f, 1.0f)





