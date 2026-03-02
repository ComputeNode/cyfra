package io.computenode.cyfra.fotonviz

import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GBufferRegion, GCodec, GExecution}
import io.computenode.cyfra.core.GBufferRegion.{AllocRegion, MapRegion}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import io.computenode.cyfra.utility.Logger.logger
import org.lwjgl.BufferUtils

import java.nio.{ByteBuffer, ByteOrder}

/** Main visualization loop that runs GExecutions with window input.
  *
  * @param window
  *   The GLFW window to render to
  * @param runtime
  *   The Cyfra runtime for GPU operations
  */
class VizLoop(val window: VizWindow)(using runtime: CyfraRuntime):

  private var frameIndex: Int = 0
  private var lastFrameTime: Long = System.nanoTime()
  private var startTime: Long = System.nanoTime()

  /** Run an infinite visualization loop.
    *
    * @param execution
    *   The GExecution pipeline to run each frame
    * @param initLayout
    *   Function to create initial layout (called with Allocation context)
    * @param updateLayout
    *   Function to update input buffer before each frame
    * @param extractPixels
    *   Function to extract pixel data from the result layout
    * @tparam Params
    *   Pipeline parameters type
    * @tparam L
    *   Layout type (must be same for init and result)
    */
  def runInfinite[Params, L: Layout](
    execution: GExecution[Params, L, L],
    params: Params,
    initLayout: Allocation ?=> L,
    updateLayout: (Allocation, InputState, L) => Unit,
    extractPixels: (Allocation, L) => ByteBuffer,
  ): Unit =
    runtime.withAllocation: allocation =>
      given Allocation = allocation
      var currentLayout = initLayout
      allocation.submitLayout(currentLayout)

      startTime = System.nanoTime()
      lastFrameTime = startTime

      while !window.shouldClose do
        window.pollEvents()

        val currentTime = System.nanoTime()
        val time = (currentTime - startTime) / 1_000_000_000.0f
        val deltaTime = (currentTime - lastFrameTime) / 1_000_000_000.0f
        lastFrameTime = currentTime

        val input = InputState.fromWindow(window, time, deltaTime, frameIndex)
        updateLayout(allocation, input, currentLayout)

        currentLayout = execution.execute(params, currentLayout)

        val pixels = extractPixels(allocation, currentLayout)
        window.renderPixels(pixels)

        frameIndex += 1

  /** Run a finite number of iterations. */
  def runN[Params, L: Layout](
    iterations: Int,
    execution: GExecution[Params, L, L],
    params: Params,
    initLayout: Allocation ?=> L,
    updateLayout: (Allocation, InputState, L) => Unit,
    extractPixels: (Allocation, L) => ByteBuffer,
  ): Unit =
    runtime.withAllocation: allocation =>
      given Allocation = allocation
      var currentLayout = initLayout
      allocation.submitLayout(currentLayout)

      startTime = System.nanoTime()
      lastFrameTime = startTime

      var i = 0
      while i < iterations && !window.shouldClose do
        window.pollEvents()

        val currentTime = System.nanoTime()
        val time = (currentTime - startTime) / 1_000_000_000.0f
        val deltaTime = (currentTime - lastFrameTime) / 1_000_000_000.0f
        lastFrameTime = currentTime

        val input = InputState.fromWindow(window, time, deltaTime, frameIndex)
        updateLayout(allocation, input, currentLayout)

        currentLayout = execution.execute(params, currentLayout)

        val pixels = extractPixels(allocation, currentLayout)
        window.renderPixels(pixels)

        frameIndex += 1
        i += 1

object VizLoop:

  /** Create a visualization loop with a new window and runtime. */
  def withWindow(width: Int, height: Int, title: String)(body: VizLoop ?=> VizWindow ?=> CyfraRuntime ?=> Unit): Unit =
    val window = VizWindow.create(width, height, title)
    val runtime = new VkCyfraRuntime()
    try
      given CyfraRuntime = runtime
      given VizWindow = window
      given VizLoop = new VizLoop(window)
      body
    finally
      runtime.close()
      window.close()

  /** Helper to create input state buffer */
  def createInputBuffer(): ByteBuffer =
    val stride = totalStride(summon[GStructSchema[InputState]])
    ByteBuffer.allocateDirect(stride).order(ByteOrder.nativeOrder())

  /** Helper to update input buffer with current state */
  def updateInputBuffer(buffer: ByteBuffer, state: InputState): Unit =
    buffer.rewind()
    summon[GCodec[InputState, InputState]].toByteBuffer(buffer, Array(state))
    buffer.rewind()
