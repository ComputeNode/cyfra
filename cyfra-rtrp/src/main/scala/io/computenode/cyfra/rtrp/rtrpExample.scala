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
import io.computenode.cyfra.vulkan.util.Util.pushStack
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.system.MemoryStack

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
    private var graphicsQueue: VkQueue = _
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
    private var commandBuffer: VkCommandBuffer = _

    private var imageAvailableSemaphore: Semaphore = _
    private var renderFinishedSemaphore: Semaphore = _
    private var inFlightFence: Fence = _

    private def run(): Unit =
        init()
        mainLoop()

    private def init(): Unit =
        windowManager = WindowManager.create().get
        context = VulkanContext.withSurfaceSupport()
        device = context.device
        graphicsQueue = context.graphicsQueue.get
        windowManager.initializeWithVulkan(context).get

        val vertShaderCode = Shader.loadShader("shaders/vert.spv")
        val fragShaderCode = Shader.loadShader("shaders/frag.spv")

        // Assuming shaders don't require special layout info for this example
        vertShader = new Shader(vertShaderCode, "main", device)
        fragShader = new Shader(fragShaderCode, "main", device)

        val result = windowManager.createWindowWithSurface()
        val (window, surface) = result.get
        surfaceManager = windowManager.getSurfaceManager().get

        presentQueue = surfaceManager.initializePresentQueue(surface).get.get

        swapchainManager = new SwapchainManager(context, surface)
        swapchain = swapchainManager.initialize(surfaceManager.getSurfaceConfig(window.id).get)

        renderPass = RenderPass(context, swapchain)

        graphicsPipeline = new GraphicsPipeline(swapchain, vertShader, fragShader, context, renderPass)

        swapchainFramebuffers = renderPass.swapchainFramebuffers

        commandPool = context.commandPool
        commandBuffer = commandPool.createCommandBuffer()

        imageAvailableSemaphore = new Semaphore(device)
        renderFinishedSemaphore = new Semaphore(device)
        inFlightFence = new Fence(device, VK_FENCE_CREATE_SIGNALED_BIT)

    def mainLoop(): Unit =
        while (!window.shouldClose)
            windowManager.pollAndDispatchEvents()
            drawFrame()

        vkDeviceWaitIdle(device.get)

    def drawFrame(): Unit =
        vkWaitForFences(device.get, inFlightFence.get, true, Long.MaxValue)
        vkResetFences(device.get, inFlightFence.get)

        pushStack: stack =>
            val pImageIndex = stack.callocInt(1)
            val result = vkAcquireNextImageKHR(device.get, swapchain.get, Long.MaxValue, imageAvailableSemaphore.get, NULL, pImageIndex)
            if result != VK_SUCCESS then throw new RuntimeException("Failed to acquire swapchain image!")
            val imageIndex = pImageIndex.get(0)
            
            commandPool.resetCommandBuffer(commandBuffer)
            renderPass.recordCommandBuffer(commandBuffer, imageIndex, graphicsPipeline.get)
        

            val submitInfo = VkSubmitInfo
                .calloc(stack)
                .sType$Default()
            val waitSemaphores = stack.longs(imageAvailableSemaphore.get)
            val waitStages = stack.ints(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
            submitInfo
                .pWaitSemaphores(waitSemaphores)
                .pWaitDstStageMask(waitStages)
                .pCommandBuffers(stack.pointers(commandBuffer))
            val signalSemaphores = stack.longs(renderFinishedSemaphore.get)
            submitInfo
                .pSignalSemaphores(signalSemaphores)

            if (vkQueueSubmit(graphicsQueue, submitInfo, inFlightFence.get) != VK_SUCCESS) then
                throw new RuntimeException("failed to submit draw command buffer!")

            val presentInfo = VkPresentInfoKHR.calloc(stack).sType$Default()
            presentInfo
                .pWaitSemaphores(signalSemaphores)
            val swapchains = stack.longs(swapchain.get)
            presentInfo
                .pSwapchains(swapchains)
                .pImageIndices(stack.ints(imageIndex))
                .pResults(null)
            vkQueuePresentKHR(presentQueue, presentInfo)

    def cleanup(): Unit =
        if (device != null && device.get != null) then
            vkDeviceWaitIdle(device.get)
        Option(renderFinishedSemaphore).foreach(_.close())
        Option(imageAvailableSemaphore).foreach(_.close())
        Option(inFlightFence).foreach(_.close())
        Option(commandPool).foreach(_.close())
        Option(graphicsPipeline).foreach(_.close())
        Option(swapchainFramebuffers).foreach(_.foreach(framebuffer =>
            vkDestroyFramebuffer(device.get, framebuffer, null)
        ))
        Option(renderPass).foreach(_.close())
        Option(swapchain).foreach(_.close())
        Option(device).foreach(_.close())
        Option(surface).foreach(_.destroy())
        Option(window).foreach(_.close())
        Option(swapchainManager).foreach(_.cleanup())
        Option(vertShader).foreach(_.close())
        Option(fragShader).foreach(_.close())
        Option(windowManager).foreach(_.shutdown())
        Option(context).foreach(_.destroy())