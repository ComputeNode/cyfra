package io.computenode.cyfra.samples.foton

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimationFunctions.{AnimationInstant, smooth}
import io.computenode.cyfra.utility.Color.hex
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.rt.animation.{AnimatedScene, AnimationRtRenderer}
import io.computenode.cyfra.foton.rt.shapes.{Plane, Shape, Sphere}
import io.computenode.cyfra.foton.rt.{Camera, Material}

import scala.concurrent.duration.DurationInt
import java.nio.file.Paths

object AnimatedSolar:

  def orbit(center: Vec3[Float32], radius: Float32, speed: Float32 = 1f, offset: Float32 = 0f)(using AnimationInstant): Vec3[Float32] =
    val a = smooth(from = 0f, to = 2f * Math.PI.toFloat, duration = 3.seconds)
    (
      center.x + sin(a * speed + offset) * radius,
      center.y + 0f - cos(a * speed + offset) * radius / 3f,
      center.z + cos(a * speed + offset) * radius
    )

  def earthOrbit()(using AnimationInstant): Vec3[Float32] =
    orbit((0f, 0f, 0f), 15f, 1f, 1.5f)

  def moonOrbit()(using AnimationInstant): Vec3[Float32] =
    orbit(earthOrbit(), 2.8f, speed = 6f, offset = 2f)

  @main
  def solar() =
    val sunMaterial = Material(
      color = (1f, 0.3f, 0.0f),
      emissive = vec3(14f),
    )

    val sun = Sphere(
      center = (0f, 0f, 0f),
      3f,
      sunMaterial
    )

    val planeMaterial = Material(
      color = (0.7f, 0.7f, 0.7f),
      emissive = vec3(0f),
    )

    val plane = Plane(
      point = vec3(0f, 10f, 0f),
      normal = vec3(0f, 1f, 0f),
      planeMaterial
    )

    val earthMaterial = Material(
      color = (0f, 0.2f, 1.0f),
      emissive = vec3(0),
      percentSpecular = 0.5f,
      specularColor = (0f, 0.2f, 1.0f) * 0.1f,
      roughness = 0.3f
    )

    val moonMaterial = Material(
      color = (0.8f, 0.8f, 0.8f),
      emissive = vec3(0f)
    )

    val scene = AnimatedScene(
      shapes = List(sun, plane,
        Sphere(earthOrbit(), 1.4f, earthMaterial),
        Sphere(moonOrbit(), 0.4f, moonMaterial)
      ),
      camera = Camera(position = (0f, -3f, smooth(from = -32f, to = -1f, 200.seconds))),
      duration = 3.seconds
    )

    val parameters = AnimationRtRenderer.Parameters(
      width = 600,
      height = 360,
      superFar = 3000f,
      pixelIterations = 10000,
      iterations = 2,
      bgColor = hex("#8FD4FD"),
      framesPerSecond = 30
    )
    val renderer = AnimationRtRenderer(parameters)
    renderer.renderFramesToDir(scene, Paths.get("solar"))
