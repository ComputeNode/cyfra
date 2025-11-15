package io.computenode.cyfra.samples.foton

import io.computenode.cyfra
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.derived
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Control.when
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.{GSeq, GStruct}
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.Parameters
import io.computenode.cyfra.foton.animation.AnimationFunctions.*
import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}
import io.computenode.cyfra.utility.Color.*

import scala.concurrent.duration.DurationInt
import java.nio.file.Paths

object AnimatedBrushedSphere:

  @main
  def brushedSphere =

    val MAX_DIST: Float32 = 1000f
    val REFLECTION_SAMPLES = 10
    val xres = 600
    val yres = 600

    case class Material(
      color: Vec3[Float32],
      emissive: Vec3[Float32],
      shininess: Float32,
      reflectionSamples: Int32 = 1,
    ) extends GStruct[Material]

    case class RayHitInfo(
      dist: Float32,
      normal: Vec3[Float32],
      material: Material,
    ) extends GStruct[RayHitInfo]

    case class Sphere(
      center: Vec3[Float32],
      radius: Float32,
      material: Material
    ) extends GStruct[Sphere]

    case class RayState(
      rayPos: Vec3[Float32],
      rayDir: Vec3[Float32],
      color: Vec3[Float32],
      random: Random
    ) extends GStruct[RayState]

    val brushedMaterial = Material(
      color = (0f, 0.3f, 1f),
      emissive = vec3(0f),
      shininess = 0.5f,
    )

    val background = Material(
      color = hex("#5BC3E3"),
      emissive = vec3(0f),
      shininess = 0f
    )

    val sphere = Sphere(
      center = (0f, 0f, 0f),
      radius = 1f,
      material = brushedMaterial
    )

    def testSphereHit(
      rayPos: Vec3[Float32],
      rayDir: Vec3[Float32],
      sphere: Sphere,
      currentHit: RayHitInfo,
    ): RayHitInfo =
      val toRay = rayPos - sphere.center
      val c2 = toRay dot toRay
      val b2 = sphere.radius * sphere.radius
      val a = -(toRay dot rayDir)
      when(a < 0f && c2 - b2 > 0f){
        currentHit
      } otherwise {
        val x = c2 - b2
        val discr = a * a - x
        when(discr >= 0f){
          val t = a - sqrt(discr)
          when(t >= 0f && t < currentHit.dist) {
            val hit = rayPos + rayDir * t
            val normal = normalize(hit - sphere.center)
            RayHitInfo(t, normal, sphere.material)
          } otherwise {
            currentHit
          }
        } otherwise {
          currentHit
        }
      }


    def testLightContribution(
      point: Vec3[Float32],
      normal: Vec3[Float32],
    )(using animationInstant: AnimationInstant): Vec3[Float32] =
      val light = vec3(2f)
      val pi = Math.PI.toFloat
      val t = smooth(from = pi * 4f/6f, to = pi * 2f/6f, 3.seconds)
      val scale = 1.6f
      val lx = cos(t) * scale
      val ly = -sin(t) * scale
      val lightPos = (lx, ly, -1f)
      val lightRay = normalize(lightPos - point)
      val contribution = lightRay dot normal
      when (contribution >= 0f) {
        light * contribution
      } otherwise {
        (0f, 0f, 0f)
      }

    def getOrthogonal(
      a: Vec3[Float32],
      b: Vec3[Float32]
    ): Vec3[Float32] =
      when ((a dot b) < 0.9999f && (a dot b) > -0.9999f) {
        normalize(a cross b)
      } otherwise {
        when (a.x > 0f || a.x < 0f) {
          (-a.y, a.x, 0f)
        } otherwise {
          (0f, -a.z, a.y)
        }
      }

    def sampleEnvironment(rayDir: Vec3[Float32])(using animationInstant: AnimationInstant): Vec3[Float32] =
      val f = smooth(from = 0f, to = 5f, duration = 5.seconds) + sin(3f  * rayDir.x) + rayDir.y * 2f
      val brightness = vec3(0.3f, 0.4f, 1f)
      val contrast = vec3(0.25f, 0.5f, 0f)
      val freq = vec3(1f)
      val offsets = vec3(0.5f, 0f, 0f)
      igPallette(brightness, contrast, freq, offsets, f)

    def traceRay(
      rayPos: Vec3[Float32],
      rayDir: Vec3[Float32],
      random: Random
    )(using animationInstant: AnimationInstant): Vec3[Float32] =
      val noHit = background.color
      val hitInfo = testSphereHit(rayPos, rayDir, sphere, RayHitInfo(MAX_DIST, vec3(0f), background))
      when(hitInfo.dist >= MAX_DIST) {
        noHit
      } otherwise {
        val hitPoint = rayDir * hitInfo.dist + rayPos
        val lightContribution = testLightContribution(hitPoint, hitInfo.normal)
        val diffuse = hitInfo.material.color mulV lightContribution
        val reflectDir = rayDir - hitInfo.normal * (hitInfo.normal dot rayDir) * 2f
        val nextRayPos = hitPoint + reflectDir * 0.001f
        val reflected = GSeq.gen(
          first = RayState(nextRayPos, reflectDir, sampleEnvironment(reflectDir) * (1f / REFLECTION_SAMPLES), random),
          next = {
            case state@RayState(rayPos, rayDir, color, random) =>
              val (rand1, angle) = random.next[Float32]
              val (rand2, scale) = rand1.next[Float32]
              val sqrScale = sqrt(scale) / 2f
              val firstBasis = getOrthogonal(rayDir, hitInfo.normal)
              val secondBasis = getOrthogonal(firstBasis, rayDir)
              val wiggle = (firstBasis * sin(angle) + secondBasis * cos(angle)) * sqrScale
              val newSample = sampleEnvironment(normalize(rayDir + wiggle))
              RayState(
                rayPos = rayPos,
                rayDir = rayDir,
                color = color + newSample * (1f / REFLECTION_SAMPLES),
                random = rand2
              )
          }
        ).limit(REFLECTION_SAMPLES).lastOr(
          RayState(vec3(0f), vec3(0f), vec3(0f), random)
        )
        mix(diffuse, reflected.color, hitInfo.material.shininess)
      }

    def sphereScene(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =
      val cz = smooth(from = -4f, to = -3f, 3.seconds)
      val camera = vec3(0f, 0f, cz)
      val cameraDir = normalize(vec3(uv.x * xres.toFloat / yres.toFloat, uv.y, 1f))
      val rand = Random((uv.x * 10479f + uv.y * 4991f).asInt.unsigned)
      val ray = traceRay(camera, cameraDir, rand)
      (ray, 1f)

    val animatedSphere = AnimatedFunction.fromCoord(sphereScene, 3.seconds)

    val renderer = AnimatedFunctionRenderer(Parameters(xres, yres, 30))
    renderer.renderFramesToDir(animatedSphere, Paths.get("brushed"))
