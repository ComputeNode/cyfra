package io.computenode.cyfra.samples.foton

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.utility.Color.*
import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}
import scala.concurrent.duration.DurationInt
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.core.{Surface, SurfaceManager, SwapChainManager}
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.render.{RenderLoopSynchronizer, RenderCommandBufferRecorder}
import io.computenode.cyfra.window.{GLFWWindowSystem, WindowEvent, WindowHandle}
import org.lwjgl.glfw.GLFW
import org.lwjgl.vulkan.VK10.*
import java.nio.file.Paths
import io.computenode.cyfra.vulkan.util.Util.pushStack
import io.computenode.cyfra.dsl.{GContext, MVPContext, GArray2DFunction, Vec4FloatMem, GStruct, UniformContext, RGBA}
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import io.computenode.cyfra.dsl.derived 
import io.computenode.cyfra.foton.animation.AnimationFunctions.AnimationInstant 
import org.lwjgl.vulkan.KHRSurface.VK_PRESENT_MODE_FIFO_KHR 

case class AnimationUniform(time: Float32) extends GStruct[AnimationUniform]
object AnimationUniform {}

object ColorGradientAnimation:
  @main def animate() =
    AnimatedFunctionRenderer(
      AnimatedFunctionRenderer.Parameters(800, 600, 30)
    ).renderFramesToDir(
      AnimatedFunction.fromCoord(rainbowGradient, 5.seconds),
      Paths.get("rainbow")
    )

  def rainbowGradient(uv: Vec2[Float32])(using inst: AnimationInstant): Vec4[Float32] =
    val colors = Array(
      hex("#FF0000"), hex("#FF7F00"), hex("#FFFF00"), hex("#00FF00"),
      hex("#0000FF"), hex("#4B0082"), hex("#9400D3"), hex("#FF0000")
    )
    val cycleDuration: Float32 = 5000f
    val cyclePosition = inst.time.mod(cycleDuration) / cycleDuration
    val numColorIntervals = (colors.length - 1).toFloat
    val scaledPosition = cyclePosition * numColorIntervals
    val colorIndex = scaledPosition.asInt
    val nextColorIndex = (colorIndex + 1).mod(colors.length)
    val colorFraction = scaledPosition - colorIndex.asFloat
    // Use chained 'when' expressions instead of foldLeft to avoid type mismatch
    def selectColor(idx: Int32): Vec3[Float32] =
      when(idx === 0) { colors(0) }
        .otherwise(when(idx === 1) { colors(1) }
        .otherwise(when(idx === 2) { colors(2) }
        .otherwise(when(idx === 3) { colors(3) }
        .otherwise(when(idx === 4) { colors(4) }
        .otherwise(when(idx === 5) { colors(5) }
        .otherwise(when(idx === 6) { colors(6) }
        .otherwise(colors(7))))))))
    val color1 = selectColor(colorIndex)
    val color2 = selectColor(nextColorIndex)
    val c = mix(color1, color2, colorFraction)
    (c.r, c.g, c.b, 1.0f)

  @main def animateRealtime(): Unit =
    new GradientRenderer(
      rainbowGradient, 800, 600, "Color Gradient Animation (GPU)", false
    ).run()

/**
 * A renderer that creates a window and sets up Vulkan resources for rendering color gradients using GContext.
 */
class GradientRenderer(
    gradientFunc: (Vec2[Float32]) => AnimationInstant ?=> Vec4[Float32],
    var width: Int = 800,
    var height: Int = 600,
    val title: String = "Color Gradient Animation (GPU)",
    val enableValidation: Boolean = false
) extends AutoCloseable {

  given gContext: GContext = new MVPContext()
  private var gpuFunction: GArray2DFunction[AnimationUniform, Vec4[Float32], Vec4[Float32]] = _
  private var outputMem: Vec4FloatMem = _
  private var currentTime: Float = 0f
  private var windowSystem: GLFWWindowSystem = _
  private var window: WindowHandle = _
  private var context: VulkanContext = _
  private var surfaceManager: SurfaceManager = _
  private var surface: Surface = _
  private var swapChainManager: SwapChainManager = _
  private var synchronizer: RenderLoopSynchronizer = _
  private var commandRecorder: RenderCommandBufferRecorder = _
  private var commandPool: CommandPool = _
  private var commandBuffers: Array[org.lwjgl.vulkan.VkCommandBuffer] = _
  private var frameCount: Int = 0
  private var lastFpsUpdateTime: Double = 0.0
  private var frameRateLimit: Double = 60.0
  private var currentFps: Double = 0.0
  private var minFps: Double = Double.MaxValue
  private var maxFps: Double = 0.0
  private var fpsUpdateInterval: Double = 0.5
  private var frameTimeHistory: List[Double] = Nil
  private val frameTimeHistorySize: Int = 60
  private var initialized = false
  private var isRunning = false
  private var lastFrameRenderTime: Double = GLFW.glfwGetTime()

  def initialize(): Boolean =
    try
      windowSystem = new GLFWWindowSystem()
      GLFW.glfwWindowHint(GLFW.GLFW_SCALE_TO_MONITOR, GLFW.GLFW_FALSE)
      GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, GLFW.GLFW_TRUE)
      if (System.getProperty("os.name").toLowerCase.contains("mac"))
        GLFW.glfwWindowHint(GLFW.GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW.GLFW_FALSE)
      window = windowSystem.createWindow(width, height, title)
      if (window == null || window.nativePtr == 0) return false
      context = new VulkanContext(enableValidation)
      surfaceManager = new SurfaceManager(context)
      surface = surfaceManager.createSurface(window.nativePtr)
      swapChainManager = new SwapChainManager(context, surface)
      if (!swapChainManager.initialize(width, height, preferredPresentMode = VK_PRESENT_MODE_FIFO_KHR)) return false
      val actualExtent = swapChainManager.getExtent
      width = actualExtent.width()
      height = actualExtent.height()
      synchronizer = new RenderLoopSynchronizer(context)
      synchronizer.initialize()
      commandRecorder = new RenderCommandBufferRecorder(context.device, context.allocator)
      commandPool = context.commandPool
      val swapChainImages = swapChainManager.getImages
      commandBuffers = Array.tabulate(swapChainImages.length)(_ => commandPool.createCommandBuffer())
      gpuFunction = GArray2DFunction(width, height, {
        case (AnimationUniform(time), (xi: Int32, yi: Int32), _) =>
          val u = (xi.asFloat - (width.toFloat / 2f)) / width.toFloat
          val v = (yi.asFloat - (height.toFloat / 2f)) / height.toFloat
          given AnimationInstant = AnimationInstant(time)
          gradientFunc((u, v))
      })
      outputMem = Vec4FloatMem(Array.fill(width * height)((0f, 0f, 0f, 1f)))
      lastFpsUpdateTime = GLFW.glfwGetTime()
      initialized = true
      isRunning = true
      true
    catch { case ex: Exception => ex.printStackTrace(); cleanup(); false }

  def renderFrame(frameIndex: Int, animationTime: Float): Boolean =
    if !initialized || shouldClose() then return false
    this.currentTime = animationTime
    val frameStartTime = GLFW.glfwGetTime()
    try
      val imageIndex = synchronizer.beginFrame(swapChainManager)
      if (imageIndex < 0) { recreateSwapChain(); return true }
      given UniformContext[AnimationUniform] = UniformContext(AnimationUniform(animationTime))
      val pixelData = try Await.result(outputMem.map(gpuFunction), scala.concurrent.duration.Duration(1, "minute"))
        catch { case _: Exception => return true }
      val commandBuffer = commandBuffers(imageIndex)
      vkResetCommandBuffer(commandBuffer, 0)
      try
        commandRecorder.recordTransferToSwapChain(
          commandBuffer, pixelData, swapChainManager.getImages.apply(imageIndex), width, height
        )
      catch { case _: Exception => return true }
      synchronizer.submitCommandBuffer(commandBuffer, imageIndex)
      val presentSuccess = synchronizer.presentFrame(
        swapChainManager, imageIndex, commandBuffer, context.computeQueue.get
      )
      if (!presentSuccess) { if (!recreateSwapChain()) return false; return true }
      val frameEndTime = GLFW.glfwGetTime()
      updateFpsStatistics(frameEndTime - frameStartTime)
      applyFrameRateLimit()
      true
    catch { case _: Exception => false }

  private def updateFpsStatistics(frameTime: Double): Unit =
    frameCount += 1
    frameTimeHistory = (frameTime :: frameTimeHistory).take(frameTimeHistorySize)
    val now = GLFW.glfwGetTime()
    val timeSinceLastUpdate = now - lastFpsUpdateTime
    if (timeSinceLastUpdate >= fpsUpdateInterval) {
      val fps = frameCount / timeSinceLastUpdate
      currentFps = fps
      minFps = math.min(minFps, fps)
      maxFps = math.max(maxFps, fps)
      val avgFrameTime = if (frameTimeHistory.nonEmpty) frameTimeHistory.sum / frameTimeHistory.size else 0.0
      val avgFps = if (avgFrameTime > 0) 1.0 / avgFrameTime else 0.0
      val colorTime = (currentTime / 1000.0f)
      GLFW.glfwSetWindowTitle(
        window.nativePtr,
        f"$title | FPS: ${currentFps.toInt} | Avg: ${avgFps.toInt} | Min: ${minFps.toInt} | Max: ${maxFps.toInt} | Time: ${colorTime}%.1fs"
      )
      frameCount = 0
      lastFpsUpdateTime = now
    }

  private def applyFrameRateLimit(): Unit =
    val targetFrameTime = 1.0 / frameRateLimit
    val elapsed = GLFW.glfwGetTime() - lastFrameRenderTime
    if (elapsed < targetFrameTime) {
      val sleepTime = (targetFrameTime - elapsed) * 1000.0
      if (sleepTime > 1.0) Try(Thread.sleep(sleepTime.toLong))
    }
    lastFrameRenderTime = GLFW.glfwGetTime()

  def setFrameRateLimit(fps: Double): Unit = frameRateLimit = math.max(1.0, fps)
  def shouldClose(): Boolean = window != null && window.nativePtr != 0 && windowSystem.shouldWindowClose(window)
  def processEvents(): Unit =
    if !initialized then return
    windowSystem.pollEvents().foreach {
      case WindowEvent.Resize(newWidth, newHeight) if newWidth > 0 && newHeight > 0 => recreateSwapChain()
      case WindowEvent.Key(GLFW.GLFW_KEY_ESCAPE, GLFW.GLFW_PRESS, _) =>
        if (window != null && window.nativePtr != 0) GLFW.glfwSetWindowShouldClose(window.nativePtr, true)
        isRunning = false
      case WindowEvent.Close => isRunning = false
      case _ =>
    }
  def getSwapChainManager: SwapChainManager = swapChainManager
  def getVulkanContext: VulkanContext = context
  def isInitialized: Boolean = initialized
  def isRendererRunning: Boolean = isRunning

  private def cleanup(): Unit =
    if (context != null && context.device != null) Try(context.device.waitIdle())
    if (synchronizer != null) { Try(synchronizer.close()); synchronizer = null }
    if (commandPool != null && commandBuffers != null) { Try(commandPool.freeCommandBuffer(commandBuffers: _*)); commandBuffers = null }
    if (swapChainManager != null) { Try(swapChainManager.destroy()); swapChainManager = null }
    if (surfaceManager != null && surface != null) surface = null
    if (surfaceManager != null) { Try(surfaceManager.destroy()); surfaceManager = null }
    if (context != null) { Try(context.destroy()); context = null }
    if (window != null && windowSystem != null) { Try(windowSystem.destroyWindow(window)); window = null }
    initialized = false
    isRunning = false

  override def close(): Unit = cleanup()

  def run(cycleDuration: Float = 5000f): Boolean =
    if (!isInitialized && !initialize()) return false
    try
      val startTime = System.currentTimeMillis()
      isRunning = true
      var loopFrameIndex = 0
      var lastEventProcessingTime = System.currentTimeMillis()
      while (isRunning && !shouldClose()) {
        val loopStartTime = System.currentTimeMillis()
        if (loopStartTime - lastEventProcessingTime > 10) {
          processEvents()
          lastEventProcessingTime = loopStartTime
          if (!isRunning || shouldClose()) return true
        }
        val animationTime = (loopStartTime - startTime) % cycleDuration
        if (!renderFrame(loopFrameIndex, animationTime)) isRunning = false
        loopFrameIndex += 1
      }
      true
    catch { case _: Exception => isRunning = false; false }
    finally cleanup()

  private def recreateCommandBuffers(): Unit =
    if (commandBuffers != null && commandPool != null) Try(commandPool.freeCommandBuffer(commandBuffers: _*))
    if (swapChainManager != null && commandPool != null) {
      val swapChainImages = swapChainManager.getImages
      commandBuffers =
        if (swapChainImages != null) Array.tabulate(swapChainImages.length)(_ => commandPool.createCommandBuffer())
        else null
    } else commandBuffers = null

  private def resetFpsStats(): Unit =
    frameCount = 0
    minFps = Double.MaxValue
    maxFps = 0.0
    currentFps = 0.0
    frameTimeHistory = Nil
    lastFpsUpdateTime = GLFW.glfwGetTime()
    lastFrameRenderTime = GLFW.glfwGetTime()

  private def recreateSwapChain(logDetails: Boolean = true): Boolean =
    if (context == null || context.device == null || window == null || window.nativePtr == 0) return false
    context.device.waitIdle()
    var success = false
    pushStack { stack =>
      val widthBuffer = stack.mallocInt(1)
      val heightBuffer = stack.mallocInt(1)
      GLFW.glfwGetFramebufferSize(window.nativePtr, widthBuffer, heightBuffer)
      val currentWidth = widthBuffer.get(0)
      val currentHeight = heightBuffer.get(0)
      if (currentWidth <= 0 || currentHeight <= 0) success = true
      else if (swapChainManager.initialize(currentWidth, currentHeight, preferredPresentMode = VK_PRESENT_MODE_FIFO_KHR)) {
        val newExtent = swapChainManager.getExtent
        width = newExtent.width()
        height = newExtent.height()
        if (width > 0 && height > 0) {
          recreateCommandBuffers()
          outputMem = Vec4FloatMem(Array.fill(width * height)((0f, 0f, 0f, 1f)))
          resetFpsStats()
          success = true
        }
      }
    }
    success
}