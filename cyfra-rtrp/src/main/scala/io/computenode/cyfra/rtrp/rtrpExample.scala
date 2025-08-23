package io.computenode.cyfra.rtrp

import io.computenode.cyfra.rtrp.graphics.{GraphicsPipeline, Shader}
import io.computenode.cyfra.rtrp.RenderPass
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.core.{Device}
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Semaphore}
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.KHRSwapchain.*
import org.lwjgl.vulkan.VK10.*
import io.computenode.cyfra.rtrp.window.core.{Window, WindowConfig}
import io.computenode.cyfra.rtrp.surface.core.{Surface, SurfaceConfig}
import io.computenode.cyfra.rtrp.surface.SurfaceManager
import io.computenode.cyfra.rtrp.window.WindowManager
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil.memPutInt
import io.computenode.cyfra.utility.Logger.logger

import scala.util.{Failure, Success}

object rtrpExample:

    def main(args: Array[String]): Unit =
        val example = new rtrpExample()
        try
            example.run()
        catch
            case e: Exception =>
                e.printStackTrace()
        finally
            example.cleanup()

class rtrpExample:
    private var context: VulkanContext = _
    private var device: Device = _
    private var queue: VkQueue = _
    private var presentQueue: VkQueue = _

    private var vertShader: Shader = _
    private var fragShader: Shader = _

    private var windowManager: WindowManager = _
    private var window: Window = _
    private var surface: Surface = _

    private var swapchainManager: SwapchainManager = _
    private var swapchain: Swapchain = _
    private var surfaceManager: SurfaceManager = _

    private var renderPass: RenderPass = _
    private var graphicsPipeline: GraphicsPipeline = _
    private var swapchainFramebuffers: Array[Long] = _

    private var commandPool: CommandPool = _
    private var commandBuffers: Seq[VkCommandBuffer] = _

    private var imageAvailableSemaphores: Seq[Semaphore] = _
    private var renderFinishedSemaphores: Seq[Semaphore] = _
    private var inFlightFences: Seq[Fence] = _
    private val MAX_FRAMES_IN_FLIGHT = 2
    private var currentFrame: Int = 0
    private var running = true

    private def init(): Unit =
        windowManager = WindowManager.create().get
        context = VulkanContext.withSurfaceSupport()
        device = context.device
        queue = context.queue.get
        windowManager.initializeWithVulkan(context).get

        val vertShaderCode = Shader.loadShader("shaders/vert.spv")
        val fragShaderCode = Shader.loadShader("shaders/frag.spv")

        // Assuming shaders don't require special layout info for this example
        vertShader = new Shader(vertShaderCode, "main", device)
        fragShader = new Shader(fragShaderCode, "main", device)

        var result = windowManager.createWindowWithSurface()
        var (w, s) = result.get
        window = w
        surface = s
        surfaceManager = windowManager.getSurfaceManager().get

        presentQueue = surfaceManager.initializePresentQueue(surface).get.get

        swapchainManager = new SwapchainManager(context, surface)
        swapchain = swapchainManager.initialize(surfaceManager.getSurfaceConfig(window.id).get)

        renderPass = RenderPass(context, swapchain)

        graphicsPipeline = new GraphicsPipeline(swapchain, vertShader, fragShader, context, renderPass)

        swapchainFramebuffers = renderPass.swapchainFramebuffers

        commandPool = context.commandPool
        commandBuffers = commandPool.createCommandBuffers(MAX_FRAMES_IN_FLIGHT)

        imageAvailableSemaphores = (1 to MAX_FRAMES_IN_FLIGHT).map{_ => new Semaphore(device)}
        renderFinishedSemaphores = (1 to MAX_FRAMES_IN_FLIGHT).map{_ => new Semaphore(device)}
        inFlightFences = (1 to MAX_FRAMES_IN_FLIGHT).map{_ => new Fence(device, VK_FENCE_CREATE_SIGNALED_BIT)}

        vkDeviceWaitIdle(device.get)

    private def destroySwapchainResources(): Unit =
        if swapchain != null then
            vkDeviceWaitIdle(device.get)
            Option(renderPass).foreach(_.destroyFramebuffers())
            Option(graphicsPipeline).foreach(_.destroy())
            Option(renderPass).foreach(_.destroy())
            Option(swapchainManager).foreach(_.destroyImageViews(swapchain))
            Option(swapchainManager).foreach(_.destroySwapchain(swapchain))
            swapchain = null

    def drawFrame(): Unit = pushStack: stack =>

        Option(inFlightFences(currentFrame)).foreach(_.block())
        Option(inFlightFences(currentFrame)).foreach(_.reset())

        val pImageIndex = stack.callocInt(1)
        val acquireResult = vkAcquireNextImageKHR(device.get, swapchain.get, Long.MaxValue, imageAvailableSemaphores(currentFrame).get, VK_NULL_HANDLE, pImageIndex)
        check(acquireResult, s"Failed to acquire swapchain image (code $acquireResult)")
        val imageIndex = pImageIndex.get(0)
        
        // Reset & record command buffer
        check(vkResetCommandBuffer(commandBuffers(currentFrame), 0), "Failed to reset command buffer")
        val framebuffer = renderPass.swapchainFramebuffers(imageIndex)
        if framebuffer == 0L then
          logger.warn(s"[WARN] Framebuffer for imageIndex=$imageIndex is null")
          return
        val recordedOk = renderPass.recordCommandBuffer(commandBuffers(currentFrame), framebuffer, imageIndex, graphicsPipeline)
        if !recordedOk then return

        // submit
        val waitSemaphores = stack.longs(imageAvailableSemaphores(currentFrame).get)
        val waitStages = stack.ints(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
        val signalSemaphores = stack.longs(renderFinishedSemaphores(currentFrame).get)
        val pCommandBuffers = stack.pointers(commandBuffers(currentFrame))

        val submitInfo = VkSubmitInfo
            .calloc(stack)
            .sType$Default()
            .pWaitSemaphores(waitSemaphores)
            .pWaitDstStageMask(waitStages)
            .pCommandBuffers(pCommandBuffers)
            .pSignalSemaphores(signalSemaphores)

        // Manually set counts; can't find any other way rn :'(
        memPutInt(submitInfo.address() + VkSubmitInfo.WAITSEMAPHORECOUNT, waitSemaphores.remaining())
        memPutInt(submitInfo.address() + VkSubmitInfo.COMMANDBUFFERCOUNT, 1)
        memPutInt(submitInfo.address() + VkSubmitInfo.SIGNALSEMAPHORECOUNT, signalSemaphores.remaining())

        check(vkQueueSubmit(queue, submitInfo, inFlightFences(currentFrame).get), "vkQueueSbmit failed")

        val pSwapchains = stack.longs(swapchain.get)
        val pImageIndices = stack.ints(imageIndex)

        val presentInfo = VkPresentInfoKHR
            .calloc(stack)
            .sType$Default()
            .pWaitSemaphores(signalSemaphores)
            .pSwapchains(pSwapchains)
            .pImageIndices(pImageIndices)

        // Manually here too :/
        memPutInt(presentInfo.address() + VkPresentInfoKHR.WAITSEMAPHORECOUNT, signalSemaphores.remaining())
        memPutInt(presentInfo.address() + VkPresentInfoKHR.SWAPCHAINCOUNT, pSwapchains.remaining())
        
        val presentResult = vkQueuePresentKHR(presentQueue, presentInfo)
        check(presentResult, s"vkQueuePresentKHR failed: $presentResult")
        currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT

    def cleanup(): Unit =
        try
            vkDeviceWaitIdle(device.get)

            destroySwapchainResources()

            Option(imageAvailableSemaphores(currentFrame)).foreach(_.destroy())
            Option(renderFinishedSemaphores(currentFrame)).foreach(_.destroy())
            Option(inFlightFences(currentFrame)).foreach(_.destroy())

            Option(commandPool).foreach(_.destroy())

            Option(surfaceManager).foreach(m => if surface != null then m.destroySurface(surface.windowId))
            
            Option(windowManager).foreach(_.destroyWindow(window))
            
            Option(vertShader).foreach(_.destroy())
            Option(fragShader).foreach(_.destroy())

            Option(context).foreach(_.destroy())
            Option(device).foreach(_.destroy())

        catch
            case t: Throwable => println(s"[cleanup] error: ${t.getMessage}")

    def mainLoop(): Unit = 
        while running do
            windowManager.pollAndDispatchEvents()

            if window.shouldClose then running = false

            if surface == null || surface.isDestroyed then running = false

            if !running then ()
            else  
                drawFrame()

    def run(): Unit = 
    try 
        init()
        mainLoop()
    finally
        cleanup()