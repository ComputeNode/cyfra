package io.computenode.cyfra.vizexamples

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.fotonviz.render.*
import io.computenode.cyfra.fotonviz.render.Vec3Ops.{vec3f, negate3}

/**
 * Animated metallic spiky sphere - demonstrating the SDF rendering DSL.
 *
 * Uses:
 *  - `SdfPrimitives.sphere` for base shape
 *  - `SdfOps.opDisplace` for spike deformation
 *  - `Easing.heartbeat` for pulsing animation
 *  - `Shading.metallic` for reflective material
 *  - `Background.vignette` for edge darkening
 */
object SdfSpikySphere:

  // ---- Configuration ----
  val WIDTH = 1200
  val HEIGHT = 900
  val CAM_DIST = 4.0f
  val FOV_DIST = 1.8f

  case class Config(
    time: Float32, mouseX: Float32, mouseY: Float32,
    camDist: Float32, fovDist: Float32, width: Float32, height: Float32,
  ) extends GStruct[Config]

  // ---- Scene-Specific SDF ----

  /** 3D sine wave displacement for spiky effect. */
  def spikeDisplacement(p: Vec3[Float32], time: Float32, intensity: Float32): Float32 =
    val freq = 6.0f
    val static = sin(p.x * freq) * sin(p.y * freq) * sin(p.z * freq)
    val animated = sin(p.x * 5.0f + time * 0.7f) * sin(p.y * 6.0f) * cos(p.z * 5.0f - time * 0.5f)
    -(static * 0.6f + animated * 0.4f) * intensity

  /** Scene SDF: sphere + displacement. */
  def sceneSdf(p: Vec3[Float32], time: Float32, intensity: Float32): Float32 =
    SdfOps.opDisplace(
      SdfPrimitives.sphere(p, radius = 1.0f),
      spikeDisplacement(p, time, intensity),
    )

  // ---- Render Program ----

  def renderProgram(runner: SdfRunner[Config]): GProgram[Int, runner.SdfLayout] =
    GProgram.static[Int, runner.SdfLayout](
      layout = n => runner.SdfLayout(GBuffer[Vec4[Float16]](n), GUniform[Config]()),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read

      GIO.when(invocId < cfg.width.asInt * cfg.height.asInt):
        // Pixel → normalized coordinates
        val px = invocId.mod(cfg.width.asInt)
        val py = invocId / cfg.width.asInt
        val u = (px.asFloat / cfg.width) * 2.0f - 1.0f
        val v = 1.0f - (py.asFloat / cfg.height) * 2.0f

        // Camera & ray
        val camPos = vec3(0.0f, 0.0f, -cfg.camDist)
        val rayDir = normalize(vec3f(u * (cfg.width / cfg.height), v, cfg.fovDist))

        // Animation: heartbeat-driven intensity
        val intensity = Easing.heartbeat(cfg.time, period = 3.0f) * 0.3f + 0.2f

        // Build SDF closure for this frame
        def sdf(p: Vec3[Float32]): Float32 = sceneSdf(p, cfg.time, intensity)

        // Raymarch
        val result = Raymarch.march(camPos, rayDir, sdf, maxSteps = 512)
        val hit = result.y > 0.5f

        // Shading
        val hitPos = Raymarch.hitPoint(camPos, rayDir, result.x)
        val normal = Raymarch.normal(hitPos, sdf)
        val viewDir = negate3(rayDir)
        val lightDir = normalize(vec3f(0.6f, 0.4f, -0.7f))
        val objColor = Shading.metallic(normal, viewDir, lightDir, baseColor = vec3(0.85f, 0.85f, 0.9f))

        // Background
        val bgColor = Background.vignette(u, v, px.asFloat, py.asFloat)

        // Composite & write (RGBA16F format)
        val color = when(hit)(objColor).otherwise(bgColor)
        GIO.write(layout.pixels, invocId, Pixels.rgba16f(color))

  // ---- Main Entry Point ----

  @main def runSdfSpikySphere(): Unit =
    val runner = SdfRunner[Config](WIDTH, HEIGHT, "Metallic Spiky Sphere")

    runner.run(
      program = renderProgram(runner),
      makeConfig = ctx => Config(
        time = ctx.time,
        mouseX = ctx.mouseX,
        mouseY = ctx.mouseY,
        camDist = CAM_DIST,
        fovDist = FOV_DIST,
        width = ctx.width.toFloat,
        height = ctx.height.toFloat,
      ),
    )
