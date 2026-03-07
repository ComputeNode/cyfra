package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GCodec, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.fotonviz.audio.AudioCapture
import io.computenode.cyfra.fotonviz.interop.InteropRenderer
import io.computenode.cyfra.runtime.{VkCyfraRuntime, VkInterop}
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import izumi.reflect.Tag

import java.nio.{ByteBuffer, ByteOrder}
import scala.reflect.ClassTag

/**
 * Frame context with audio frequency bands for music visualizers.
 * FFT is computed on CPU once per frame, band levels are passed as uniforms.
 *
 * @param time        Seconds since start
 * @param mouseX      Normalized mouse X [-1, 1]
 * @param mouseY      Normalized mouse Y [-1, 1]
 * @param width       Window width
 * @param height      Window height
 * @param bass        Bass level (20-250 Hz), 0-1 range
 * @param lowMid      Low-mid level (250-1000 Hz), 0-1 range  
 * @param highMid     High-mid level (1000-4000 Hz), 0-1 range
 * @param treble      Treble level (4000-16000 Hz), 0-1 range
 */
case class AudioFrameContext(
  time: Float,
  mouseX: Float,
  mouseY: Float,
  width: Int,
  height: Int,
  bass: Float,
  lowMid: Float,
  highMid: Float,
  treble: Float,
)

/**
 * Runner for audio-reactive SDF visualizations.
 * FFT is computed on CPU, frequency band levels are passed as uniforms in Config.
 *
 * @tparam C Config type (must extend GStruct)
 */
class AudioSdfRunner[C <: GStruct[C]: GStructSchema: ClassTag: Tag](
  val width: Int,
  val height: Int,
  val title: String,
  val warmupFrames: Int = 2,
  val fpsReportInterval: Int = 60,
):

  /**
   * Layout for audio-reactive SDF rendering:
   * - pixels: RGBA16F output
   * - config: User config uniform (includes audio band levels)
   */
  case class AudioSdfLayout(
    pixels: GBuffer[Vec4[Float16]],
    config: GUniform[C],
  ) derives Layout

  type PostProcess = GProgram[Int, AudioSdfLayout]

  /**
   * Run the audio-reactive visualization.
   * FFT is computed on CPU, frequency band levels passed via makeConfig.
   *
   * @param program       The GPU render program
   * @param makeConfig    Function to create config from frame context (includes band levels)
   * @param postProcess   Sequence of post-processing programs
   * @param audioMixerName Optional audio mixer name (None = default input, Some("stereo mix") for loopback)
   */
  def run(
    program: GProgram[Int, AudioSdfLayout],
    makeConfig: AudioFrameContext => C,
    postProcess: Seq[PostProcess] = Seq.empty,
    audioMixerName: Option[String] = None,
  ): Unit =
    println(s"=== $title === (${width}x$height)")
    println(s"Available audio inputs: ${AudioCapture.listAudioInputs.mkString(", ")}")

    val renderer = InteropRenderer(width, height, title)
    val runtime = new VkCyfraRuntime()
    given CyfraRuntime = runtime

    // Start audio capture (FFT computed on CPU)
    val audioCapture = new AudioCapture()
    val audioStarted = audioCapture.start(audioMixerName)
    if !audioStarted then
      println("WARNING: Audio capture failed to start. Visualization will run without audio.")

    try
      runtime.withAllocation: allocation =>
        given Allocation = allocation

        val sharedBuffer = renderer.initSharedBuffer(runtime.vulkanDevice, runtime.vulkanPhysicalDevice)

        // Config buffer
        val configStride = totalStride(summon[GStructSchema[C]])
        val configBuffer = ByteBuffer.allocateDirect(configStride).order(ByteOrder.nativeOrder())
        val configUniform = GUniform[C](configBuffer)

        // Pixel buffer: RGBA16F = 8 bytes per pixel
        val pixelBuffer = ByteBuffer.allocateDirect(width * height * 8).order(ByteOrder.nativeOrder())
        pixelBuffer.rewind()

        val layout = AudioSdfLayout(
          GBuffer[Vec4[Float16]](pixelBuffer),
          configUniform,
        )
        allocation.submitLayout(layout)

        def updateConfig(ctx: AudioFrameContext): Unit =
          val cfg = makeConfig(ctx)
          summon[GCodec[C, C]].toByteBuffer(configBuffer, Array(cfg))
          configBuffer.rewind()
          configUniform.write(configBuffer)

        // Warm-up
        val defaultCtx = AudioFrameContext(0f, 0f, 0f, width, height, 0f, 0f, 0f, 0f)
        for _ <- 0 until warmupFrames do
          updateConfig(defaultCtx)
          allocation.submitLayout(layout)
          program.execute(width * height, layout)
          postProcess.foreach(_.execute(width * height, layout))

        val (vkQueue, commandPool) = VkInterop.getQueueAndCommandPool(allocation)
        val startTime = System.nanoTime()
        var frameIndex = 0

        while !renderer.shouldClose do
          renderer.pollEvents()
          renderer.updateMousePosition()

          val time = ((System.nanoTime() - startTime) / 1e9).toFloat
          val ctx = AudioFrameContext(
            time,
            renderer.normalizedMouseX,
            renderer.normalizedMouseY,
            width,
            height,
            audioCapture.bass,
            audioCapture.lowMid,
            audioCapture.highMid,
            audioCapture.treble,
          )

          updateConfig(ctx)
          program.execute(width * height, layout)

          postProcess.foreach(_.execute(width * height, layout))

          allocation.submitLayout(layout)
          sharedBuffer.copyFrom(VkInterop.getBufferHandle(layout.pixels), vkQueue, commandPool)
          renderer.render()

          frameIndex += 1
          if fpsReportInterval > 0 && frameIndex % fpsReportInterval == 0 then
            val elapsed = (System.nanoTime() - startTime) / 1e9
            // println(f"Frame $frameIndex, FPS: ${frameIndex / elapsed}%.1f")
    finally
      audioCapture.stop()
      org.lwjgl.vulkan.VK10.vkDeviceWaitIdle(runtime.vulkanDevice)
      renderer.close()
      runtime.close()

object AudioSdfRunner:
  def apply[C <: GStruct[C]: GStructSchema: ClassTag: Tag](
    width: Int,
    height: Int,
    title: String,
  ): AudioSdfRunner[C] = new AudioSdfRunner[C](width, height, title)

  // --- Post-processing effects for AudioSdfRunner ---

  private def fract(x: Float32): Float32 = x - x.asInt.asFloat

  private def wangHash(seed: UInt32): UInt32 =
    val s1 = (seed ^ 61) ^ (seed >> 16)
    val s2 = s1 * 9
    val s3 = s2 ^ (s2 >> 4)
    val s4 = s3 * 0x27d4eb2d
    s4 ^ (s4 >> 15)

  private def hash3d(px: Float32, py: Float32, frame: Float32): Float32 =
    val ix = px.asInt.unsigned
    val iy = py.asInt.unsigned
    val iframe = frame.asInt.unsigned
    val seed = ix * 374761393 + iy * 668265263 + iframe * 1013904223
    val hashed = wangHash(seed)
    hashed.asFloat / 4294967296.0f

  private def luminance(r: Float32, g: Float32, b: Float32): Float32 =
    r * 0.2126f + g * 0.7152f + b * 0.0722f

  /**
   * Film look post-process for AudioSdfRunner.
   * Combines grain, vignette, contrast, and color grading.
   */
  def filmLook[C <: GStruct[C]: GStructSchema: Tag](
    runner: AudioSdfRunner[C],
    grainStrength: Float = 0.04f,
    vignetteStrength: Float = 0.3f,
    contrast: Float = 1.05f,
    saturation: Float = 0.95f,
  )(
    getWidth: C => Int32,
    getHeight: C => Int32,
    getTime: C => Float32,
  ): runner.PostProcess =
    GProgram.static[Int, runner.AudioSdfLayout](
      layout = n => runner.AudioSdfLayout(
        GBuffer[Vec4[Float16]](n),
        GUniform[C](),
      ),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read
      val width = getWidth(cfg)
      val height = getHeight(cfg)
      val time = getTime(cfg)
      val totalPixels = runner.width * runner.height

      GIO.when(invocId < totalPixels):
        val px = invocId.mod(width)
        val py = invocId / width

        val color = layout.pixels.read(invocId)
        var r = color.x.asFloat32
        var g = color.y.asFloat32
        var b = color.z.asFloat32
        val a = color.w.asFloat32

        // 1. Film grain
        val grainX = px.asFloat
        val grainY = py.asFloat
        val frameOffset = (time * 24.0f).asInt.asFloat

        val noise1 = hash3d(grainX, grainY, frameOffset)
        val noise2 = hash3d(grainX * 2.1f + 100.0f, grainY * 2.1f + 50.0f, frameOffset)
        val combinedNoise = noise1 * 0.7f + noise2 * 0.3f
        val triangularNoise = (combinedNoise - 0.5f) * 2.0f

        val lum = luminance(r, g, b)
        val shadowFactor = 1.0f - lum * 0.5f
        val grainAmount = triangularNoise * grainStrength * shadowFactor

        r = clamp(r + grainAmount, 0.0f, 1.0f)
        g = clamp(g + grainAmount, 0.0f, 1.0f)
        b = clamp(b + grainAmount, 0.0f, 1.0f)

        // 2. Vignette
        val uvX = px.asFloat / width.asFloat
        val uvY = py.asFloat / height.asFloat
        val cx = uvX - 0.5f
        val cy = uvY - 0.5f
        val vignetteDist = sqrt(cx * cx + cy * cy) * 1.414f
        val vignetteFactor = 1.0f - vignetteDist * vignetteDist * vignetteStrength

        r = r * vignetteFactor
        g = g * vignetteFactor
        b = b * vignetteFactor

        // 3. Contrast
        r = (r - 0.5f) * contrast + 0.5f
        g = (g - 0.5f) * contrast + 0.5f
        b = (b - 0.5f) * contrast + 0.5f

        // 4. Saturation
        val lumAfter = luminance(r, g, b)
        r = lumAfter + (r - lumAfter) * saturation
        g = lumAfter + (g - lumAfter) * saturation
        b = lumAfter + (b - lumAfter) * saturation

        // Clamp and write
        layout.pixels.write(invocId, Pixels.rgba16f(
          clamp(r, 0.0f, 1.0f),
          clamp(g, 0.0f, 1.0f),
          clamp(b, 0.0f, 1.0f),
          a,
        ))
