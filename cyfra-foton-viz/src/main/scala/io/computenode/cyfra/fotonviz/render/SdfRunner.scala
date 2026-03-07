package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GCodec, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.fotonviz.interop.InteropRenderer
import io.computenode.cyfra.runtime.{VkCyfraRuntime, VkInterop}
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride

import izumi.reflect.Tag

import java.nio.{ByteBuffer, ByteOrder}
import scala.reflect.ClassTag

/**
 * Frame context passed to the config update function.
 *
 * @param time   Seconds since start
 * @param mouseX Normalized mouse X [-1, 1]
 * @param mouseY Normalized mouse Y [-1, 1]
 * @param width  Window width
 * @param height Window height
 */
case class FrameContext(
  time: Float,
  mouseX: Float,
  mouseY: Float,
  width: Int,
  height: Int,
)

/**
 * Generic runner for SDF-based visualizations.
 *
 * Handles all boilerplate: runtime setup, buffer management, render loop, FPS tracking.
 *
 * @tparam C Config type (must extend GStruct)
 */
class SdfRunner[C <: GStruct[C]: GStructSchema: ClassTag: Tag](
  val width: Int,
  val height: Int,
  val title: String,
  val warmupFrames: Int = 2,
  val fpsReportInterval: Int = 60,
):

  /** Layout for SDF rendering: RGBA16F pixels + config uniform. */
  case class SdfLayout(pixels: GBuffer[Vec4[Float16]], config: GUniform[C]) derives Layout

  /** Type for post-process programs that operate on the same layout. */
  type PostProcess = GProgram[Int, SdfLayout]

  /**
   * Run the visualization.
   *
   * @param program      The GPU render program
   * @param makeConfig   Function to create config from frame context
   * @param postProcess  Sequence of post-processing programs to run after rendering
   */
  def run(
    program: GProgram[Int, SdfLayout],
    makeConfig: FrameContext => C,
    postProcess: Seq[PostProcess] = Seq.empty,
  ): Unit =
    println(s"=== $title === (${width}x$height)")
    val renderer = InteropRenderer(width, height, title)
    val runtime = new VkCyfraRuntime()
    given CyfraRuntime = runtime

    try
      runtime.withAllocation: allocation =>
        given Allocation = allocation

        val sharedBuffer = renderer.initSharedBuffer(runtime.vulkanDevice, runtime.vulkanPhysicalDevice)

        // Config buffer
        val configStride = totalStride(summon[GStructSchema[C]])
        val configBuffer = ByteBuffer.allocateDirect(configStride).order(ByteOrder.nativeOrder())
        val configUniform = GUniform[C](configBuffer)

        // Pixel buffer: RGBA16F = 8 bytes per pixel (4 channels × Float16)
        val pixelBuffer = ByteBuffer.allocateDirect(width * height * 8).order(ByteOrder.nativeOrder())
        pixelBuffer.rewind()

        val layout = SdfLayout(GBuffer[Vec4[Float16]](pixelBuffer), configUniform)
        allocation.submitLayout(layout)

        def updateConfig(ctx: FrameContext): Unit =
          val cfg = makeConfig(ctx)
          summon[GCodec[C, C]].toByteBuffer(configBuffer, Array(cfg))
          configBuffer.rewind()
          configUniform.write(configBuffer)

        // Warm-up
        val defaultCtx = FrameContext(0f, 0f, 0f, width, height)
        for _ <- 0 until warmupFrames do
          updateConfig(defaultCtx)
          allocation.submitLayout(layout)
          program.execute(width * height, layout)
          // Run post-processing during warmup too
          postProcess.foreach(_.execute(width * height, layout))

        val (vkQueue, commandPool) = VkInterop.getQueueAndCommandPool(allocation)
        val startTime = System.nanoTime()
        var frameIndex = 0

        while !renderer.shouldClose do
          renderer.pollEvents()
          renderer.updateMousePosition()

          val time = ((System.nanoTime() - startTime) / 1e9).toFloat
          val ctx = FrameContext(time, renderer.normalizedMouseX, renderer.normalizedMouseY, width, height)

          updateConfig(ctx)
          program.execute(width * height, layout)

          // Run post-processing programs in sequence
          postProcess.foreach(_.execute(width * height, layout))

          allocation.submitLayout(layout)
          sharedBuffer.copyFrom(VkInterop.getBufferHandle(layout.pixels), vkQueue, commandPool)
          renderer.render()

          frameIndex += 1
          if fpsReportInterval > 0 && frameIndex % fpsReportInterval == 0 then
            val elapsed = (System.nanoTime() - startTime) / 1e9
            println(f"Frame $frameIndex, FPS: ${frameIndex / elapsed}%.1f")
    finally
      org.lwjgl.vulkan.VK10.vkDeviceWaitIdle(runtime.vulkanDevice)
      renderer.close()
      runtime.close()

object SdfRunner:
  /** Create a runner with default settings. */
  def apply[C <: GStruct[C]: GStructSchema: ClassTag: Tag](
    width: Int,
    height: Int,
    title: String,
  ): SdfRunner[C] = new SdfRunner[C](width, height, title)
