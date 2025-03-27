package io.computenode.cyfra.samples.foton

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimationFunctions.smooth
import io.computenode.cyfra.utility.Color.hex
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.rt.animation.{AnimatedScene, AnimationRtRenderer}
import io.computenode.cyfra.foton.rt.shapes.{Plane, Shape, Sphere}
import io.computenode.cyfra.foton.rt.{Camera, Material}
import scala.concurrent.duration.DurationInt

import java.nio.file.Paths
import io.computenode.cyfra.foton.animation.AnimationFunctions.*
import io.computenode.cyfra.foton.rt.shapes.Cylinder
import io.computenode.cyfra.dsl.Control.when

object Orbit:
  @main
  def orbitraytrace() =

    val jupiterMaterial = Material(
      color = vec3(0.1f, 0.7f, 0.6f),              
      emissive = vec3(0.02f),                      
      percentSpecular = 0.4f,                      
      specularColor = vec3(0.1f, 0.085f, 0.04f), 
      roughness = 0.3f                            
    )
    val ringMaterial = Material(
      color = vec3(0.6f, 0.6f, 0.7f),        
      emissive = vec3(0.1f),                 
      percentSpecular = 0.8f,                
      specularColor = vec3(0.5f), 
      roughness = 0.1f,                      
      refractionChance = 0.4f,              
      indexOfRefraction = 1.1f,             
      refractionRoughness = 0.1f         
    )

    val sunMaterial = Material(
      color = vec3(1f, 0.2f, 0.05f),
      emissive = vec3(4f, 0.4f, 0.1f)     
    )
    val sunMaterial1Color1 = vec3(1f, 0.2f, 0.05f)
    val sunMaterial1Color2 = vec3(0f, 0f, 1f)

    val lightMaterial = Material(
      color = (1f, 0.3f, 0.3f),
      emissive = vec3(4f)
    )
       

    val staticShapes: List[Shape] = List(
      
      Sphere((0f, -140f, 10f), 50f, lightMaterial),
    )
   
    val scene = AnimatedScene(
      shapes = staticShapes ::: List(
        Sphere(orbit((0f, 1f, 14f), 8f, 60.seconds, 0.millis, 0f, 300f), 1f, jupiterMaterial),
        Sphere((-1f, 0.5f, 14f),5f, Material(colorChange(sunMaterial1Color1, sunMaterial1Color2, 4f), emissive = vec3(0f, 0.4f, 0.1f))),
        Cylinder(orbit((0f, 1f, 14f), 8f, 60.seconds, 0.millis, 0f, 300f),2f, 0f, ringMaterial)
      ),
      camera = Camera(position = (0f, 0f, -50f)),
      duration = 5.seconds
    )

    val parameters = AnimationRtRenderer.Parameters(
      width = 640,
      height = 480,
      superFar = 300f,
      pixelIterations = 500,
      iterations = 2,
      bgColor = hex("#000000"),
      framesPerSecond = 60
    )
    val renderer = AnimationRtRenderer(parameters)
    renderer.renderFramesToDir(scene, Paths.get("Orbit"))

// Renderable with ffmpeg -framerate 30 -pattern_type sequence -start_number 01 -i frame%02d.png -s:v 1920x1080 -c:v libx264 -crf 17 -pix_fmt yuv420p output.mp4

// ffmpeg -t 3 -i output.mp4 -vf "fps=30,scale=720:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
