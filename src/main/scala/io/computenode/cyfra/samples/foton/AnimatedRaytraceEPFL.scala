package io.computenode.cyfra.samples.foton

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimationFunctions.smooth
import io.computenode.cyfra.utility.Color.hex
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.rt.animation.{AnimatedScene, AnimationRtRenderer}
import io.computenode.cyfra.foton.rt.shapes.{Plane, Shape, Sphere, Box}
import io.computenode.cyfra.foton.rt.{Camera, Material}
import scala.concurrent.duration.DurationInt

import java.nio.file.Paths

object BoxRaytrace:
  @main
  def raytraceEPFL() =
    val lightMaterial = Material(
      color = (1f, 0.3f, 0.3f),
      emissive = vec3(40f)
    )

    val floorMaterial = Material(
      color = vec3(0.5f),
      emissive = vec3(0f),
      roughness = 0.9f
    )
    val wallMaterial = Material(
      color = (0.4f, 0.0f, 0.0f),        
      emissive = vec3(0f),                    
      percentSpecular = 0f,                
      specularColor = (1f, 0.3f, 0.3f) * 0.1f, 
      roughness = 1f                           
    )
    
    val x = 0f
    val y =  -1.5f
    val z = 15f

    val staticShapes: List[Shape] = List(

        //E
        Box((x + 0f, y + 0f, z + 0f), (x + 1f, y + 5f, z + 1f), wallMaterial),
        Box((x + 1f, y + 4f, z + 0f), (x + 3f, y + 5f, z + 1f), wallMaterial),
        Box((x + 1f, y + 2f, z + 0f), (x + 3f, y + 3f, z + 1f), wallMaterial),
        Box((x + 1f, y + 0f, z + 0f), (x + 3f, y + 1f, z + 1f), wallMaterial),

        //P
        Box((x + 4f, y + 0f, z + 0f), (x + 5f, y + 5f, z + 1f), wallMaterial),
        Box((x + 5f, y + 0f, z + 0f), (x + 7f, y + 1f, z + 1f), wallMaterial),
        Box((x + 7f, y + 0f, z + 0f), (x + 8f, y + 2f, z + 1f), wallMaterial),
        Box((x + 5f, y + 2f, z + 0f), (x + 8f, y + 3f, z + 1f), wallMaterial),

        // F
        Box((x + 9f,  y + 0f, z + 0f), (x + 10f, y + 5f, z + 1f), wallMaterial),
        Box((x + 10f, y + 0f, z + 0f), (x + 12f, y + 1f, z + 1f), wallMaterial),
        Box((x + 10f, y + 2f, z + 0f), (x + 12f, y + 3f, z + 1f), wallMaterial),

        //L
        Box((x + 13f, y + 0f, z + 0f), (x + 14f, y + 5f, z + 1f), wallMaterial),
        Box((x + 14f, y + 4f, z + 0f), (x + 16f, y + 5f, z + 1f), wallMaterial),

        // Light
        Sphere((0f, -140f, 10f), 50f, lightMaterial),

        // Floor
        Plane((0f, 3.5f, 0f), (0f, 1f, 0f), floorMaterial),
    )

    val scene = AnimatedScene(
      shapes = staticShapes,
      camera = Camera(position = (smooth(from = -20f, to = 40f, duration = 3.seconds), 0f, -1f)),
      duration = 3.seconds
    )

    val parameters = AnimationRtRenderer.Parameters(
      width = 640,
      height = 480,
      superFar = 300f,
      pixelIterations = 10000,
      iterations = 2,
      bgColor = hex("#ADD8E6"),
      framesPerSecond = 30
    )
    val renderer = AnimationRtRenderer(parameters)
    renderer.renderFramesToDir(scene, Paths.get("raytraceEPFL"))

// Renderable with ffmpeg -framerate 30 -pattern_type sequence -start_number 01 -i frame%02d.png -s:v 1920x1080 -c:v libx264 -crf 17 -pix_fmt yuv420p output.mp4

// ffmpeg -t 3 -i output.mp4 -vf "fps=30,scale=720:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
