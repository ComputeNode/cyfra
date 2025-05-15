package io.computenode.samples.cyfra.foton

import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.animation.AnimationFunctions.smooth
import io.computenode.cyfra.foton.rt.animation.{AnimatedScene, AnimationRtRenderer}
import io.computenode.cyfra.foton.rt.shapes.{Plane, Shape, Sphere}
import io.computenode.cyfra.foton.rt.{Camera, Material}
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.given
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Color.hex
import io.computenode.cyfra.dsl.given

import java.nio.file.Paths
import scala.concurrent.duration.DurationInt

object AnimatedRaytrace:
  @main
  def raytrace() =
    val sphereMaterial = Material(
      color = (1f, 0.3f, 0.3f),
      emissive = vec3(0f),
      percentSpecular = 0.5f,
      specularColor = (1f, 0.3f, 0.3f) * 0.1f,
      roughness = 0.2f
    )

    val sphere2Material = Material(
      color = (1f, 0.3f, 0.6f),
      emissive = vec3(0f),
      percentSpecular = 0.1f,
      specularColor = (1f, 0.3f, 0.6f) * 0.1f,
      roughness = 0.1f,
      refractionChance = 0.9f,
      indexOfRefraction = 1.5f,
      refractionRoughness = 0.1f,
    )
    val sphere3Material = Material(
      color = (1f, 0.6f, 0.3f),
      emissive = vec3(0f),
      percentSpecular = 0.5f,
      specularColor = (1f, 0.6f, 0.3f) * 0.1f,
      roughness = 0.2f
    )
    val sphere4Material = Material(
      color = (1f, 0.2f, 0.2f),
      emissive = vec3(0f),
      percentSpecular = 0.5f,
      specularColor = (1f, 0.2f, 0.2f) * 0.1f,
      roughness = 0.2f
    )

    val boxMaterial = Material(
      color = (0.3f, 0.3f, 1f),
      emissive = vec3(0f),
      percentSpecular = 0.5f,
      specularColor = (0.3f, 0.3f, 1f) * 0.1f,
      roughness = 0.1f
    )

    val lightMaterial = Material(
      color = (1f, 0.3f, 0.3f),
      emissive = vec3(40f)
    )

    val floorMaterial = Material(
      color = vec3(0.5f),
      emissive = vec3(0f),
      roughness = 0.9f
    )

    val staticShapes: List[Shape] = List(
      // Spheres
      Sphere((-1f, 0.5f, 14f), 3f, sphereMaterial),
      Sphere((-3f, 2.5f, 10f), 1f, sphere3Material),
      Sphere((9f, -1.5f, 18f), 5f, sphere4Material),
      // Light
      Sphere((-140f, -140f, 10f), 50f, lightMaterial),
      // Floor
      Plane((0f, 3.5f, 0f), (0f, 1f, 0f), floorMaterial),
    )

    val scene = AnimatedScene(
      shapes = staticShapes ::: List(
        Sphere(
          center = (3f, smooth(from = -5f, to = 1.5f, duration = 2.seconds), 10f),
          2f,
          sphere2Material
        ),
      ),
      camera = Camera(position = (2f, 0f, smooth(from = -5f, to = -1f, 2.seconds))),
      duration = 3.seconds
    )

    val parameters = AnimationRtRenderer.Parameters(
      width = 1920,
      height = 1080,
      superFar = 300f,
      pixelIterations = 10000,
      iterations = 2,
      bgColor = hex("#ADD8E6"),
      framesPerSecond = 30
    )
    val renderer = AnimationRtRenderer(parameters)
    renderer.renderFramesToDir(scene, Paths.get("output"))

// Renderable with ffmpeg -framerate 30 -pattern_type sequence -start_number 01 -i frame%02d.png -s:v 1920x1080 -c:v libx264 -crf 17 -pix_fmt yuv420p output.mp4

// ffmpeg -t 3 -i output.mp4 -vf "fps=30,scale=720:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
