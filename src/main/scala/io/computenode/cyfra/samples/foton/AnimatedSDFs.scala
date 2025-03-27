package io.computenode.cyfra.samples.foton

import io.computenode.cyfra
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.GSeq
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.Parameters
import io.computenode.cyfra.foton.animation.AnimationFunctions.*
import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}
import io.computenode.cyfra.utility.Color.*
import io.computenode.cyfra.utility.Math3D.*

import scala.concurrent.duration.DurationInt
import java.nio.file.Paths

object AnimatedSDFs:
  @main
  def sdfs() =
    val MAX_STEPS = 100
    val MAX_DIST: Float32 = 200f
    val MIN_DIST: Float32 = 0.0002f        
    val VISIBILITY_THRESHOLD: Float32 = 100f
    // val SPHERE_POSITION: Vec3[Float32] = (-0.5f, -0.5f, 0f)
    // val SPHERE_RADIUS: Float32 = 0.2f
    val BOX_POSITION: Vec3[Float32] = (0f, -0.1f, 0f)
    val BOX_SIZE: Vec3[Float32] = (0.2f, 0.4f, 0.2f)
    val TORUS_POSITION: Vec3[Float32] = (0f, -0.7f, 0f)
    val TORUS_RADII: Vec2[Float32] = (0.25f, 0.07f)   

    // def sdfSphere(center: Vec3[Float32], radius: Float32, position: Vec3[Float32]): Float32 = 
    //   vecDistance(center, position) - radius
    
    def smoothMin(a: Float32, b: Float32, k: Float32): Float32 =
      val diff = abs(a - b)
      val h = max(k - diff, 0f) / k
      min(a, b) - h*h*h*k*1f/6f

    def smoothMinBlendFactor(a: Float32, b: Float32, k: Float32): Float32 =
      val h = clamp(0.5f + 0.5f * (b - a) / k, 0f, 1f)
      h * h * (3f - 2f * h)

    def lerp(a: Float32, b: Float32, t: Float32): Float32 =
      a * (1f - t) + b * t

    def sdfBox(center: Vec3[Float32], halfSideLength: Vec3[Float32], position: Vec3[Float32]): Float32 = 
      val boxPos = position - center
      val q = abs(boxPos) - halfSideLength
      length(max(q, (0f, 0f, 0f))) + min(max(q.x, max(q.y, q.z)), 0f)

    def sdfTorus(center: Vec3[Float32], radii: Vec2[Float32], position: Vec3[Float32]): Float32 =   
      val torusPos = position - center      
      val q: Vec2[Float32] = (length((torusPos.x, torusPos.z)) - radii.x, torusPos.y)
      length(q) - radii.y

    def getDistance(position: Vec3[Float32]): Float32 =      
      val torus = sdfTorus(TORUS_POSITION, TORUS_RADII, position)
      val box = sdfBox(BOX_POSITION, BOX_SIZE, position)        
      smoothMin(torus, box, 0.5f)      

    def getColor(position: Vec3[Float32]): Vec3[Float32] =      
      val torus = sdfTorus(TORUS_POSITION, TORUS_RADII, position)
      val box = sdfBox(BOX_POSITION, BOX_SIZE, position)        

      val boxColor: Vec3[Float32] = (0f, 0.5f, 1f)
      val torusColor: Vec3[Float32] = (1f, 0.25f, 0.25f)
      val factor = smoothMinBlendFactor(torus, box, 0.5f)

      (
        lerp(boxColor.x, torusColor.x, factor),
        lerp(boxColor.y, torusColor.y, factor),
        lerp(boxColor.z, torusColor.z, factor)
      )

    def getNormal(position: Vec3[Float32]): Vec3[Float32] =
      val epsilon = 0.01f
      normalize(
        getDistance((position.x + epsilon, position.y, position.z)),
        getDistance((position.x, position.y + epsilon, position.z)),
        getDistance((position.x, position.y, position.z + epsilon)),
      )

    def getLight(position: Vec3[Float32]): Float32 =
      val lightPos: Vec3[Float32] = (0.2f, -2f, -1f)
      val lightDir = normalize(position - lightPos)
      val normal = getNormal(position)
          
      -(normal.x * lightDir.x + normal.y * lightDir.y + normal.z * lightDir.z)

    def rayMarch(origin: Vec3[Float32], rayDirection: Vec3[Float32])(using AnimationInstant): Float32 =
      GSeq.gen[Float32](getDistance(origin), next = dist => {
        val currPosition = origin + (rayDirection * dist)
        val currDistance = getDistance(currPosition)
        dist + currDistance
      })
      .takeWhile(dist => dist > MIN_DIST && dist < MAX_DIST)
      .limit(MAX_STEPS)
      .lastOr(MAX_DIST)

    def mainImage(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =      
      val focalDistance = 0.6f
      val camera = (1f, -1.8f, -1.3f)      
      val lookAt = (0f, -0.4f, 0f)
      
      val forward = normalize(lookAt - camera)
      val right = normalize(io.computenode.cyfra.dsl.Functions.cross((0f, 1f, 0f), forward))
      val up = io.computenode.cyfra.dsl.Functions.cross(forward, right)
      val rayDirection = normalize(forward + 
                        right * uv.x + 
                        up * uv.y)


      // val rayDirection = (uv.x, uv.y, focalDistance)
      // val distance = rayMarch(camera, rayDirection)
      val distance = rayMarch(camera, rayDirection)
            
      when(distance < VISIBILITY_THRESHOLD){
        val p = camera + (rayDirection * distance)
        val light = getLight(p)
        val color = getColor(p)
        val result = color * (light + 0.3f)

        (result.x, result.y, result.z, 1f)
      }.otherwise{
        (0.5f, 0.5f, 0.5f, 1f)
      }      

    val animation = AnimatedFunction.fromCoord(mainImage, 1.seconds)

    val renderer = AnimatedFunctionRenderer(Parameters(1024, 1024, 1))
    renderer.renderFramesToDir(animation, Paths.get("output"))

