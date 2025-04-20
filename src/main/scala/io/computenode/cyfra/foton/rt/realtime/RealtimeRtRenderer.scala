package io.computenode.cyfra.foton.rt.realtime

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.{GArray2DFunction, GContext, GStruct, MVPContext, RGBA, Random, UniformContext, Vec4FloatMem}
import io.computenode.cyfra.foton.rt.{Camera, RtRenderer, Scene}
import io.computenode.cyfra.utility.Color.*
import io.computenode.cyfra.utility.Math3D.*
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.core.{SurfaceManager, SwapChainManager}
import io.computenode.cyfra.vulkan.render.RenderCommandBufferRecorder
import io.computenode.cyfra.vulkan.render.RenderLoopSynchronizer
import io.computenode.cyfra.window.{GLFWWindowSystem, WindowEvent, WindowHandle}
import org.lwjgl.glfw.GLFW
import io.computenode.cyfra.dsl.derived
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Control.*
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.system.MemoryStack.*
import io.computenode.cyfra.dsl.{*, given}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import java.util.concurrent.atomic.AtomicReference

//eal-time ray tracing renderer that renders progressively to a Vulkan swap chain
 
class RealtimeRtRenderer(params: RealtimeRtRenderer.Parameters) extends RtRenderer(params) {

  private val context = new VulkanContext(params.enableValidation)
  private val windowSystem = new GLFWWindowSystem()
  
  private val frameAccumulationHistory = params.initialAccumulationHistory
  private var currentFrame = 0
  private var isRunning = false
  private var isPaused = false

  // Adaptive quality control
  private var currentPixelIterations = params.initialPixelIterations
  private var lastFrameTime = 0L
  private var adaptiveQualityEnabled = params.adaptiveQuality
  
  // Interaction state
  private val sceneRef = new AtomicReference[Scene](null)
  private val cameraRef = new AtomicReference[Camera](null)
  
  // For quality/framerate measurements
  private var frameTimeHistory = List.empty[Double]
  
//art the renderer and create a window

  def start(initialScene: Scene): Unit = {
    // Initialize window and Vulkan resources
    val window = windowSystem.createWindow(params.width, params.height, params.windowTitle)
    if (window.nativePtr == 0) {
      throw new RuntimeException("Failed to create window")
    }
    
    // Create surface and swap chain
    val surfaceManager = new SurfaceManager(context)
    val surface = surfaceManager.createSurface(window.nativePtr)
    val swapChainManager = new SwapChainManager(context, surface)
    swapChainManager.initialize(params.width, params.height)
    
    // Create synchronization and command recording utilities
    val synchronizer = new RenderLoopSynchronizer(context)
    synchronizer.initialize()
    
    val commandRecorder = new RenderCommandBufferRecorder(context.device, context.allocator)
    
    // Set initial scene
    sceneRef.set(initialScene)
    cameraRef.set(initialScene.camera)
    
    // Prepare rendering resources
    val commandPool = context.commandPool
    val swapChainImages = swapChainManager.getImages
    val commandBuffers = Array.tabulate(swapChainImages.length)(i => commandPool.createCommandBuffer())
    
    // Allocate initial memory for the rendered image
    val initialMem = Array.fill(params.width * params.height)((0f, 0f, 0f, 1f))
    var currentImage = initialMem
    var accumulator = initialMem.map(c => (c._1, c._2, c._3, 0f)) // RGBA with 0 in alpha for sample count
    
    // Main rendering loop
    isRunning = true
    var frameStartTime = System.currentTimeMillis()
    
    try {
      while (isRunning && !windowSystem.shouldWindowClose(window)) {
        val newFrameStartTime = System.currentTimeMillis()
        val deltaTimeMs = newFrameStartTime - frameStartTime
        val deltaTime = deltaTimeMs / 1000.0f
        frameStartTime = newFrameStartTime
        
        // Process window events
        val events = windowSystem.pollEvents()
        for (event <- events) {
          processEvent(event, window)
        }
        
        // Check if we need to reset the accumulation buffer
        val currentCamera = cameraRef.get()
        val cameraChanged = currentCamera != initialScene.camera
        if (cameraChanged) {
          // Reset accumulation when camera changes
          accumulator = initialMem.map(c => (c._1, c._2, c._3, 0f))
          currentFrame = 0
        }
        
        // Begin a new frame (wait for previous frame and acquire an image)
        val imageIndex = synchronizer.beginFrame(swapChainManager)
        
        // If image acquisition failed, recreate swap chain
        if (imageIndex >= 0) {
          // Only render a new sample if not paused
          if (!isPaused) {
            // Render a new frame or sample
            val scene = sceneRef.get()
            val renderedFrame = renderNextSample(scene, currentFrame, accumulator)
            
            // Update accumulation buffer and current image
            currentImage = renderedFrame
            
            // Increment frame counter
            currentFrame += 1
            
            // Check if we should adapt quality settings
            if (adaptiveQualityEnabled && currentFrame % params.adaptiveFrameInterval == 0) {
              adaptQualitySettings(deltaTime)
            }
          }
          
          // Get command buffer for this image
          val commandBuffer = commandBuffers(imageIndex)
          
          // Record commands to transfer image data to swap chain
          commandRecorder.recordTransferToSwapChain(
            commandBuffer,
            currentImage,
            swapChainManager.getImages.apply(imageIndex),
            params.width,
            params.height
          )
          
          // Submit command buffer with synchronization
          synchronizer.submitCommandBuffer(commandBuffer, imageIndex)
          
          // Present the rendered image
          val presentSuccess = synchronizer.presentFrame(swapChainManager, imageIndex, commandBuffer, context.computeQueue.get)
          if (!presentSuccess) {
            // Recreate swap chain if needed (e.g., window resized)
            context.device.waitIdle()
            swapChainManager.initialize(params.width, params.height)
          }
        }
        
        // Update frame time statistics
        updateFrameStats(deltaTime)
        
        // Keep a steady frame rate if required
        if (params.limitFrameRate) {
          val targetFrameTime = 1000.0 / params.targetFPS
          val elapsedMs = System.currentTimeMillis() - frameStartTime
          val sleepTime = Math.max(0, targetFrameTime.toLong - elapsedMs)
          if (sleepTime > 0) {
            Thread.sleep(sleepTime)
          }
        }
      }
    } finally {
      // Clean up resources
      context.device.waitIdle()
      synchronizer.close()
      swapChainManager.destroy()
      surfaceManager.destroy()
      windowSystem.destroyWindow(window)
      context.destroy()
    }
  }
  
  // Render the next progressive sample
   
  private def renderNextSample(scene: Scene, frame: Int, accumulator: Array[RGBA]): Array[RGBA] = {
    // Create function for this frame
    val fn = createRenderFunction(scene, frame, currentPixelIterations)
    
    // Render the frame
    val iterationMem = Vec4FloatMem(accumulator)
    val result = UniformContext.withUniform(RealtimeRtRenderer.RealtimeIteration(frame)) {
      Await.result(iterationMem.map(fn), 1.second)
    }
    
    // Return the result
    result
  }
  
  // Create the render function for a specific frame
   
  private def createRenderFunction(
      scene: Scene, 
      frame: Int,
      pixelIterations: Int
  ): GArray2DFunction[RealtimeRtRenderer.RealtimeIteration, Vec4[Float32], Vec4[Float32]] = {
    GArray2DFunction(params.width, params.height, {
      case (RealtimeRtRenderer.RealtimeIteration(frame), (xi: Int32, yi: Int32), accumulatedFrame) =>
        // Extract accumulated color and sample count
        val currentAccumulatedValue: Vec4[Float32] = accumulatedFrame.at(xi, yi)
        val r: Float32 = currentAccumulatedValue.r
        val g: Float32 = currentAccumulatedValue.g
        val b: Float32 = currentAccumulatedValue.b
        val sampleCount: Float32 = currentAccumulatedValue.a
        
        val samples = sampleCount
        
        when (samples >= frameAccumulationHistory) {
                    currentAccumulatedValue
        }.otherwise {
          val rngSeed = xi * 1973 + yi * 9277 + frame * 26699 | 1

          // Generate the color for this frame
          val newSampleColor = GSeq.gen(
            first = RealtimeRtRenderer.RenderIteration((0f, 0f, 0f), Random(rngSeed.unsigned)), 
            next = { iteration =>
              val (random2, wiggleX) = iteration.random.next[Float32]
              val (random3, wiggleY) = random2.next[Float32]
              
              // Calculate ray direction with aspect ratio correction
              val aspectRatio = params.width.toFloat / params.height.toFloat
              val x = ((xi.asFloat + wiggleX) / params.width.toFloat) * 2f - 1f
              val y = (((yi.asFloat + wiggleY) / params.height.toFloat) * 2f - 1f) / aspectRatio
              
              // Use camera from the scene
              val camera = scene.camera
              val rayPosition = camera.position
              val cameraDist = 1.0f / tan(params.fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
              val rayTarget = (x, y, cameraDist) addV rayPosition
              
              val rayDir = normalize(rayTarget - rayPosition)
              val rtResult = bounceRay(rayPosition, rayDir, random3, scene)
              
              // Apply background color to rays that hit nothing
              val withBg = vclamp(
                rtResult.color + (SRGBToLinear(params.bgColor) mulV rtResult.throughput), 
                0.0f, 
                20.0f
              )
              
              // Return the iteration with the new color
              RealtimeRtRenderer.RenderIteration(withBg, rtResult.random) 
            }
          ).limit(pixelIterations)
           .fold((0f, 0f, 0f), { 
             case (acc, RealtimeRtRenderer.RenderIteration(color, _)) => 
               acc + (color * (1.0f / pixelIterations.toFloat)) 
           })
          
          // Convert to sRGB
          val colorCorrected = linearToSRGB(newSampleColor)
          
          // Accumulate with previous samples
          val totalSamples = samples + 1f
          val accumulationWeight = samples / totalSamples
          val newWeight = 1f / totalSamples
          
          val accumulatedR = r * accumulationWeight + colorCorrected.r * newWeight // Use .r
          val accumulatedG = g * accumulationWeight + colorCorrected.g * newWeight // Use .g
          val accumulatedB = b * accumulationWeight + colorCorrected.b * newWeight // Use .b
          
          (accumulatedR, accumulatedG, accumulatedB, totalSamples)
        } 
    })
  }
  
  // Process window and input events
   
  private def processEvent(event: WindowEvent, window: WindowHandle): Unit = {
    event match {
      case WindowEvent.Resize(newWidth, newHeight) if newWidth > 0 && newHeight > 0 =>
        // Handle window resize
        params.width = newWidth
        params.height = newHeight
        
      case WindowEvent.Key(GLFW.GLFW_KEY_ESCAPE, GLFW.GLFW_PRESS, _) =>
        // Exit on Escape key
        isRunning = false
        
      case WindowEvent.Key(GLFW.GLFW_KEY_SPACE, GLFW.GLFW_PRESS, _) =>
        // Toggle pause on Space key
        isPaused = !isPaused
        
      case WindowEvent.Key(GLFW.GLFW_KEY_R, GLFW.GLFW_PRESS, _) =>
        // Reset accumulation on R key
        currentFrame = 0
        
      case WindowEvent.Key(GLFW.GLFW_KEY_Q, GLFW.GLFW_PRESS, _) =>
        // Decrease quality
        currentPixelIterations = Math.max(1, currentPixelIterations / 2)
        println(s"Quality reduced: $currentPixelIterations samples per pixel")
        
      case WindowEvent.Key(GLFW.GLFW_KEY_E, GLFW.GLFW_PRESS, _) =>
        // Increase quality
        currentPixelIterations = Math.min(params.maxPixelIterations, currentPixelIterations * 2)
        println(s"Quality increased: $currentPixelIterations samples per pixel")
        
      case WindowEvent.Key(GLFW.GLFW_KEY_A, GLFW.GLFW_PRESS | GLFW.GLFW_REPEAT, _) =>
        // Move camera left
        updateCamera(camera => camera.copy(
          position = camera.position addV (-0.1f, 0f, 0f)
        ))
        
      case WindowEvent.Key(GLFW.GLFW_KEY_D, GLFW.GLFW_PRESS | GLFW.GLFW_REPEAT, _) =>
        // Move camera right
        updateCamera(camera => camera.copy(
          position = camera.position addV (0.1f, 0f, 0f)
        ))
        
      case WindowEvent.Key(GLFW.GLFW_KEY_W, GLFW.GLFW_PRESS | GLFW.GLFW_REPEAT, _) =>
        // Move camera forward
        updateCamera(camera => camera.copy(
          position = camera.position addV (0f, 0f, 0.1f)
        ))
        
      case WindowEvent.Key(GLFW.GLFW_KEY_S, GLFW.GLFW_PRESS | GLFW.GLFW_REPEAT, _) =>
        // Move camera backward
        updateCamera(camera => camera.copy(
          position = camera.position addV (0f, 0f, -0.1f)
        ))
        
      case _ => // Ignore other events
    }
  }
  
  // Update the camera and mark for accumulation reset
   
  private def updateCamera(updater: Camera => Camera): Unit = {
    val currentCamera = cameraRef.get()
    val newCamera = updater(currentCamera)
    cameraRef.set(newCamera)
    
    // Update the scene with the new camera
    val currentScene = sceneRef.get()
    val newScene = Scene(currentScene.shapes, newCamera)
    sceneRef.set(newScene)
    
    // Reset accumulation
    currentFrame = 0
  }
  
  // Update frame time statistics
   
  private def updateFrameStats(deltaTime: Double): Unit = {
    // Keep only the last 60 frames for statistics
    frameTimeHistory = (deltaTime :: frameTimeHistory).take(60)
    
    // Calculate average frame time
    val avgFrameTime = frameTimeHistory.sum / frameTimeHistory.size
    val fps = 1.0 / avgFrameTime
    
    // Update window title with stats every 30 frames
    if (currentFrame % 30 == 0) {
      GLFW.glfwSetWindowTitle(0, // Assuming only one window
        f"${params.windowTitle} | Frame: $currentFrame | FPS: ${fps.toInt} | " +
        f"Samples: $currentPixelIterations | ${if (isPaused) "PAUSED" else "Running"}"
      )
    }
  }
  
  // Adapt quality settings based on frame rate
   
  private def adaptQualitySettings(deltaTime: Double): Unit = {
    val fps = 1.0 / deltaTime
    val targetFPS = params.targetFPS
    
    // Calculate quality adjustment based on frame rate
    if (fps < targetFPS * 0.8 && currentPixelIterations > 1) {
      // Frame rate too low, reduce quality
      currentPixelIterations = Math.max(1, currentPixelIterations / 2)
    } else if (fps > targetFPS * 1.2 && currentPixelIterations < params.maxPixelIterations) {
      // Frame rate good, can increase quality
      currentPixelIterations = Math.min(params.maxPixelIterations, currentPixelIterations * 2)
    }
  }
  
  // Stop the renderer
   
  def stop(): Unit = {
    isRunning = false
  }
  
  // Update the scene during rendering
  def updateScene(scene: Scene): Unit = {
    sceneRef.set(scene)
    cameraRef.set(scene.camera)
    currentFrame = 0  // Reset accumulation
  }
}

object RealtimeRtRenderer {
  // Parameters specific to real-time ray tracing
  class Parameters(
    var width: Int,
    var height: Int,
    val windowTitle: String = "Real-time Ray Tracer",
    val fovDeg: Float = 60.0f,
    val superFar: Float = 1000.0f,
    val maxBounces: Int = 4,  // Lower than offline renderer for performance
    val initialPixelIterations: Int = 1,
    val maxPixelIterations: Int = 64,
    val iterations: Int = 1,  // Always 1 for real-time
    val bgColor: (Float, Float, Float) = (0.2f, 0.2f, 0.2f),
    val initialAccumulationHistory: Float = 64,  // Max frames to accumulate
    val targetFPS: Double = 30.0,
    val limitFrameRate: Boolean = true,
    val adaptiveQuality: Boolean = true,
    val adaptiveFrameInterval: Int = 10,  // Check every 10 frames
    val enableValidation: Boolean = false
  ) extends RtRenderer.Parameters:
    override val pixelIterations: Int = initialPixelIterations
  
  // Uniform for progressive rendering
  case class RealtimeIteration(frame: Int32) extends GStruct[RealtimeIteration] 
  
  // Moved from createRenderFunction
  case class RenderIteration(color: Vec3[Float32], random: Random) extends GStruct[RenderIteration] 

  // Start rendering with default parameters
  def render(scene: Scene, width: Int = 800, height: Int = 600): RealtimeRtRenderer = {
    val params = new Parameters(width, height)
    val renderer = new RealtimeRtRenderer(params)
    
    // Start the renderer in a separate thread
    Future {
      renderer.start(scene)
    }
    
    renderer
  }
}