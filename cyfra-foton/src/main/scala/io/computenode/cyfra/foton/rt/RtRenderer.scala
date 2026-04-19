package io.computenode.cyfra.foton.rt

import io.computenode.cyfra
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.library.*
import io.computenode.cyfra.foton.library.Math3D.fresnelReflectAmount
import io.computenode.cyfra.core.expression.types.Float32.given
import io.computenode.cyfra.foton.library.given
import io.computenode.cyfra.foton.library.Math3D.*
import io.computenode.cyfra.foton.library.Color.*
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class RtRenderer(params: RtRenderer.Parameters):

  given ExecutionContext = Implicits.global

  private case class RayTraceState(
    rayPos: Vec3[Float32],
    rayDir: Vec3[Float32],
    color: Vec3[Float32],
    throughput: Vec3[Float32],
    random: Random,
    finished: Bool = false,
  )

  private val applyRefractionThroughput: pure[(state: RayTraceState, testResult: RayHitInfo), Vec3[Float32]] = pure: x =>
    val (state, testResult) = x
    when(testResult.fromInside):
      state.throughput * exp[Vec3[Float32]](-testResult.material.refractionColor * testResult.dist)
    .otherwise:
      state.throughput

  private val calculateSpecularChance: pure[(state: RayTraceState, testResult: RayHitInfo), Float32] = pure: x =>
    val (state, testResult) = x
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

  private val getRefractionChance: pure[(state: RayTraceState, testResult: RayHitInfo, specularChance: Float32), Float32] = pure: x =>
    val (state, testResult, specularChance) = x
    when(specularChance > 0.0f):
      testResult.material.refractionChance * ((1.0f - specularChance) / (1.0f - testResult.material.percentSpecular))
    .otherwise:
      testResult.material.refractionChance

  private case class RayAction(doSpecular: Float32, doRefraction: Float32, rayProbability: Float32)
  private def getRayAction(state: RayTraceState, testResult: RayHitInfo, random: Random): (RayAction, Random) =
    val specularChance = calculateSpecularChance(state, testResult)
    val refractionChance = getRefractionChance(state, testResult, specularChance)
    val (nextRandom, rayRoll) = random.next[Float32]
    val doSpecular = when[Float32](specularChance > 0.0f && rayRoll < specularChance):
      1.0f
    .otherwise(0.0f)
    val doRefraction = when[Float32](refractionChance > 0.0f && doSpecular === 0.0f && rayRoll < specularChance + refractionChance):
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
  private val getNextRayPos: pure[(rayPos: Vec3[Float32], rayDir: Vec3[Float32], testResult: RayHitInfo, doRefraction: Float32), Vec3[Float32]] =
    pure: x =>
      val (rayPos, rayDir, testResult, doRefraction) = x
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
        Vec3(testResult.material.refractionRoughness * testResult.material.refractionRoughness),
      ),
    )
    (refractionRayDir, random2)

  private val getThroughput: pure[
    (testResult: RayHitInfo, doSpecular: Float32, doRefraction: Float32, rayProbability: Float32, refractedThroughput: Vec3[Float32]),
    Vec3[Float32],
  ] = pure: x =>
    val (testResult, doSpecular, doRefraction, rayProbability, refractedThroughput) = x
    val nextThroughput = when(doRefraction === 0.0f):
      refractedThroughput * mix[Float32, Vec3[Float32]](testResult.material.color, testResult.material.specularColor, Vec3(doSpecular))
    .otherwise:
      refractedThroughput
    nextThroughput * (1.0f / rayProbability)

  private def bounceRay(startRayPos: Vec3[Float32], startRayDir: Vec3[Float32], random: Random, scene: Scene): RayTraceState =
    val initState = RayTraceState(startRayPos, startRayDir, Vec3(0f, 0f, 0f), Vec3(1f, 1f, 1f), random)
    GSeq
      .gen[RayTraceState](
        first = initState,
        next =
          case state @ RayTraceState(rayPos, rayDir, color, throughput, random, _) =>
            val noHit = RayHitInfo(params.superFar, Vec3(0f), Material.Zero)
            val testResult: RayHitInfo = scene.rayTest(rayPos, rayDir, noHit)

            when(testResult.dist < params.superFar):
              val refractedThroughput = applyRefractionThroughput(state, testResult)

              val (RayAction(doSpecular, doRefraction, rayProbability), random2) = getRayAction(state, testResult, random)

              val nextRayPos = getNextRayPos(rayPos, rayDir, testResult, doRefraction)

              val (random3, randomVec1) = random2.next[Vec3[Float32]]
              val diffuseRayDir = normalize(testResult.normal + randomVec1)
              val specularRayDirPerfect = reflect(rayDir, testResult.normal)
              val specularRayDir =
                normalize(mix(specularRayDirPerfect, diffuseRayDir, Vec3(testResult.material.roughness * testResult.material.roughness)))

              val (refractionRayDir, random4) = getRefractionRayDir(rayDir, testResult, random3)

              val rayDirSpecular = mix(diffuseRayDir, specularRayDir, Vec3(doSpecular))
              val rayDirRefracted = mix(rayDirSpecular, refractionRayDir, Vec3(doRefraction))

              val nextColor = (refractedThroughput * testResult.material.emissive) + color

              val throughputRayProb = getThroughput(testResult, doSpecular, doRefraction, rayProbability, refractedThroughput)

              RayTraceState(nextRayPos, rayDirRefracted, nextColor, throughputRayProb, random4)
            .otherwise:
              RayTraceState(rayPos, rayDir, color, throughput, random, true),
      )
      .limit(params.maxBounces)
      .takeWhile(!_.finished)
      .lastOr(initState)

  def renderFrame(xi: Int32, yi: Int32, frame: Int32, lastFrame: GArray2D[Vec4[Float32]], scene: Scene): Vec4[Float32] =
    val rngSeed = xi * 1973 + yi * 9277 + frame * 26699 | 1
    case class RenderIteration(color: Vec3[Float32], random: Random)
    val color =
      GSeq
        .gen(
          first = RenderIteration(Vec3(0f, 0f, 0f), Random(UInt32(rngSeed))),
          next = { case RenderIteration(_, random) =>
            val (random2, wiggleX) = random.next[Float32]
            val (random3, wiggleY) = random2.next[Float32]
            val aspectRatio = params.width.toFloat / params.height.toFloat
            val x = ((xi.asFloat + wiggleX) / params.width.toFloat) * 2f - 1f
            val y = (((yi.asFloat + wiggleY) / params.height.toFloat) * 2f - 1f) / aspectRatio

            val rayPosition = scene.camera.position
            val cameraDist = 1.0f / tan(params.fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
            val rayTarget = Vec3(x, y, cameraDist) + rayPosition

            val rayDir = normalize(rayTarget - rayPosition)
            val rtResult = bounceRay(rayPosition, rayDir, random3, scene)
            val withBg = clamp(rtResult.color + (SRGBToLinear(Vec3(params.bgColor)) * rtResult.throughput), Vec3(0.0f), Vec3(20.0f))
            RenderIteration(withBg, rtResult.random)
          },
        )
        .limit(params.pixelIterations)
        .fold(Vec3(0f, 0f, 0f), { case (acc, RenderIteration(color, _)) => acc + (color * (1.0f / params.pixelIterations.toFloat)) })

    val colorCorrected = linearToSRGB(color)

    when(frame === 0):
      Vec4(colorCorrected, 1.0f)
    .otherwise:
      mix[Float32, Vec4[Float32]](lastFrame.at(xi, yi), Vec4(colorCorrected, 1.0f), Vec4(1.0f / (frame.asFloat + 1f)))

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

  case class RayHitInfo(dist: Float32, normal: Vec3[Float32], material: Material, fromInside: Bool = false)

  val MinRayHitTime = 0.01f
