package io.computenode.cyfra.foton.rt

import io.computenode.cyfra
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.collections.{GArray2D, GSeq}
import io.computenode.cyfra.dsl.control.Pure.pure
import io.computenode.cyfra.dsl.library.Color.*
import io.computenode.cyfra.dsl.library.Math3D.*
import io.computenode.cyfra.dsl.library.Random
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.core.aalegacy.GContext

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class RtRenderer(params: RtRenderer.Parameters):

  given GContext = new GContext()

  given ExecutionContext = Implicits.global

  private case class RayTraceState(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    color: Vec3[Float32],
    throughput: Vec3[Float32],
    random: Random,
    finished: GBoolean = false,
  ) extends GStruct[RayTraceState]

  private def applyRefractionThroughput(state: RayTraceState, testResult: RayHitInfo) = pure:
    when(testResult.fromInside):
      state.throughput mulV exp[Vec3[Float32]](-testResult.material.refractionColor * testResult.dist)
    .otherwise:
      state.throughput

  private def calculateSpecularChance(state: RayTraceState, testResult: RayHitInfo) = pure:
    when(testResult.material.percentSpecular > 0.0f):
      val material = testResult.material
      fresnelReflectAmount(
        when(testResult.fromInside)(material.indexOfRefraction).otherwise(1.0f),
        when(!testResult.fromInside)(material.indexOfRefraction).otherwise(1.0f),
        state.rayDir,
        testResult.normal,
        material.percentSpecular,
        1.0f,
      )
    .otherwise:
      0f

  private def getRefractionChance(state: RayTraceState, testResult: RayHitInfo, specularChance: Float32) = pure:
    when(specularChance > 0.0f):
      testResult.material.refractionChance * ((1.0f - specularChance) / (1.0f - testResult.material.percentSpecular))
    .otherwise:
      testResult.material.refractionChance

  private case class RayAction(doSpecular: Float32, doRefraction: Float32, rayProbability: Float32)
  private def getRayAction(state: RayTraceState, testResult: RayHitInfo, random: Random): (RayAction, Random) =
    val specularChance = calculateSpecularChance(state, testResult)
    val refractionChance = getRefractionChance(state, testResult, specularChance)
    val (nextRandom, rayRoll) = random.next[Float32]
    val doSpecular = when(specularChance > 0.0f && rayRoll < specularChance):
      1.0f
    .otherwise(0.0f)
    val doRefraction = when(refractionChance > 0.0f && doSpecular === 0.0f && rayRoll < specularChance + refractionChance):
      1.0f
    .otherwise(0.0f)

    val rayProbability = when(doSpecular === 1.0f):
      specularChance
    .elseWhen(doRefraction === 1.0f):
      refractionChance
    .otherwise:
      1.0f - (specularChance + refractionChance)

    (RayAction(doSpecular, doRefraction, max(rayProbability, 0.01f)), nextRandom)

  private val rayPosNormalNudge = 0.01f
  private def getNextRayPos(rayPos: Vec3[Float32], rayDir: Vec3[Float32], testResult: RayHitInfo, doRefraction: Float32) = pure:
    when(doRefraction =~= 1.0f):
      (rayPos + rayDir * testResult.dist) - (testResult.normal * rayPosNormalNudge)
    .otherwise:
      (rayPos + rayDir * testResult.dist) + (testResult.normal * rayPosNormalNudge)

  private def getRefractionRayDir(rayDir: Vec3[Float32], testResult: RayHitInfo, random: Random) =
    val (random2, randomVec) = random.next[Vec3[Float32]]
    val refractionRayDirPerfect =
      refract(
        rayDir,
        testResult.normal,
        when(testResult.fromInside)(testResult.material.indexOfRefraction).otherwise(1.0f / testResult.material.indexOfRefraction),
      )
    val refractionRayDir = normalize(
      mix(
        refractionRayDirPerfect,
        normalize(-testResult.normal + randomVec),
        testResult.material.refractionRoughness * testResult.material.refractionRoughness,
      ),
    )
    (refractionRayDir, random2)

  private def getThroughput(
    testResult: RayHitInfo,
    doSpecular: Float32,
    doRefraction: Float32,
    rayProbability: Float32,
    refractedThroughput: Vec3[Float32],
  ) = pure:
    val nextThroughput = when(doRefraction === 0.0f):
      refractedThroughput mulV mix[Vec3[Float32]](testResult.material.color, testResult.material.specularColor, doSpecular)
    .otherwise:
      refractedThroughput
    nextThroughput * (1.0f / rayProbability)

  private def bounceRay(startRayPos: Vec3[Float32], startRayDir: Vec3[Float32], random: Random, scene: Scene): RayTraceState =
    val initState = RayTraceState(startRayPos, startRayDir, (0f, 0f, 0f), (1f, 1f, 1f), random)
    GSeq
      .gen[RayTraceState](
        first = initState,
        next =
          case state @ RayTraceState(rayPos, rayDir, color, throughput, random, _) =>
            val noHit = RayHitInfo(params.superFar, vec3(0f), Material.Zero)
            val testResult: RayHitInfo = scene.rayTest(rayPos, rayDir, noHit)

            when(testResult.dist < params.superFar):
              val refractedThroughput = applyRefractionThroughput(state, testResult)

              val (RayAction(doSpecular, doRefraction, rayProbability), random2) = getRayAction(state, testResult, random)

              val nextRayPos = getNextRayPos(rayPos, rayDir, testResult, doRefraction)

              val (random3, randomVec1) = random2.next[Vec3[Float32]]
              val diffuseRayDir = normalize(testResult.normal + randomVec1)
              val specularRayDirPerfect = reflect(rayDir, testResult.normal)
              val specularRayDir = normalize(mix(specularRayDirPerfect, diffuseRayDir, testResult.material.roughness * testResult.material.roughness))

              val (refractionRayDir, random4) = getRefractionRayDir(rayDir, testResult, random3)

              val rayDirSpecular = mix(diffuseRayDir, specularRayDir, doSpecular)
              val rayDirRefracted = mix(rayDirSpecular, refractionRayDir, doRefraction)

              val nextColor = (refractedThroughput mulV testResult.material.emissive) addV color

              val throughputRayProb = getThroughput(testResult, doSpecular, doRefraction, rayProbability, refractedThroughput)

              RayTraceState(nextRayPos, rayDirRefracted, nextColor, throughputRayProb, random4)
            .otherwise:
              RayTraceState(rayPos, rayDir, color, throughput, random, true),
      )
      .limit(params.maxBounces)
      .takeWhile(!_.finished)
      .lastOr(initState)

  def renderFrame(xi: Int32, yi: Int32, frame: Int32, lastFrame: GArray2D[Vec4[Float32]], scene: Scene) =
    val rngSeed = xi * 1973 + yi * 9277 + frame * 26699 | 1
    case class RenderIteration(color: Vec3[Float32], random: Random) extends GStruct[RenderIteration]
    val color =
      GSeq
        .gen(
          first = RenderIteration((0f, 0f, 0f), Random(rngSeed.unsigned)),
          next = { case RenderIteration(_, random) =>
            val (random2, wiggleX) = random.next[Float32]
            val (random3, wiggleY) = random2.next[Float32]
            val aspectRatio = params.width.toFloat / params.height.toFloat
            val x = ((xi.asFloat + wiggleX) / params.width.toFloat) * 2f - 1f
            val y = (((yi.asFloat + wiggleY) / params.height.toFloat) * 2f - 1f) / aspectRatio

            val rayPosition = scene.camera.position
            val cameraDist = 1.0f / tan(params.fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
            val rayTarget = (x, y, cameraDist) addV rayPosition

            val rayDir = normalize(rayTarget - rayPosition)
            val rtResult = bounceRay(rayPosition, rayDir, random3, scene)
            val withBg = vclamp(rtResult.color + (SRGBToLinear(params.bgColor) mulV rtResult.throughput), 0.0f, 20.0f)
            RenderIteration(withBg, rtResult.random)
          },
        )
        .limit(params.pixelIterations)
        .fold((0f, 0f, 0f), { case (acc, RenderIteration(color, _)) => acc + (color * (1.0f / params.pixelIterations.toFloat)) })

    val colorCorrected = linearToSRGB(color)

    when(frame === 0):
      (colorCorrected, 1.0f)
    .otherwise:
      mix(lastFrame.at(xi, yi), (colorCorrected, 1.0f), vec4(1.0f / (frame.asFloat + 1f)))

object RtRenderer:
  trait Parameters:
    def width: Int
    def height: Int
    def fovDeg: Float
    def superFar: Float
    def maxBounces: Int
    def pixelIterations: Int
    def iterations: Int
    def bgColor: (Float, Float, Float)

  case class RayHitInfo(dist: Float32, normal: Vec3[Float32], material: Material, fromInside: GBoolean = false) extends GStruct[RayHitInfo]

  val MinRayHitTime = 0.01f
