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
import org.joml.{Vector2f, Vector3f}
import java.nio.ByteBuffer
import org.lwjgl.BufferUtils
import io.computenode.cyfra.vulkan.memory.Buffer
import org.lwjgl.util.vma.Vma.*

case class Vertex(pos: Vector2f, color: Vector3f)

object Vertex{
    val SIZEOF: Int = (2 + 3) * 4 // pos(2*float) + color(3*float)
    val OFFSETOF_POS: Int = 0
    val OFFSETOF_COLOR: Int = 2 * 4

    def toByteBuffer(vertices: Array[Vertex]): ByteBuffer = {
        val buffer = BufferUtils.createByteBuffer(vertices.length * SIZEOF)
        for (vertex <- vertices) {
            buffer.putFloat(vertex.pos.x)
            buffer.putFloat(vertex.pos.y)
            buffer.putFloat(vertex.color.x)
            buffer.putFloat(vertex.color.y)
            buffer.putFloat(vertex.color.z)
        }
        buffer.rewind()
    }
}

object rtrpExample:

    def main(args: Array[String]): Unit =
        val example = new rtrpExample()
        try
            example.run()
        catch
            case e: Exception =>
                e.printStackTrace()

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
    private var vertexBuffer: Buffer = _
    private var vertexCount: Int = 0
    private var indexBuffer: Buffer = _
    private var indexCount: Int = 0

    private val vertices = Array(
        Vertex(new Vector2f(-0.5f, -0.5f), new Vector3f(1.0f, 0.0f, 0.0f)),
        Vertex(new Vector2f(0.5f, -0.5f), new Vector3f(0.0f, 1.0f, 0.0f)),
        Vertex(new Vector2f(0.5f, 0.5f), new Vector3f(0.0f, 0.0f, 1.0f)),
        Vertex(new Vector2f(-0.5f, 0.5f), new Vector3f(1.0f, 1.0f, 1.0f))
    )

    private val indices = Array[Short](
        0, 1, 2, 2, 3, 0
    )

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

    private def recreateSwapchain(): Unit =
        vkDeviceWaitIdle(device.get)

        destroySwapchainResources()

        swapchain = swapchainManager.initialize(surfaceManager.getSurfaceConfig(window.id).get)
        renderPass = RenderPass(context, swapchain)
        graphicsPipeline = new GraphicsPipeline(swapchain, vertShader, fragShader, context, renderPass)
        swapchainFramebuffers = renderPass.swapchainFramebuffers
        
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
        commandPool = context.commandPool
        createVertexBuffer()
        createIndexBuffer()
        presentQueue = surfaceManager.initializePresentQueue(surface).get.get

        swapchainManager = new SwapchainManager(context, surface)
        swapchain = swapchainManager.initialize(surfaceManager.getSurfaceConfig(window.id).get)

        renderPass = RenderPass(context, swapchain)

        graphicsPipeline = new GraphicsPipeline(swapchain, vertShader, fragShader, context, renderPass)

        swapchainFramebuffers = renderPass.swapchainFramebuffers

        commandBuffers = commandPool.createCommandBuffers(MAX_FRAMES_IN_FLIGHT)

        imageAvailableSemaphores = (1 to MAX_FRAMES_IN_FLIGHT).map{_ => new Semaphore(device)}
        renderFinishedSemaphores = (1 to MAX_FRAMES_IN_FLIGHT).map{_ => new Semaphore(device)}
        inFlightFences = (1 to MAX_FRAMES_IN_FLIGHT).map{_ => new Fence(device, VK_FENCE_CREATE_SIGNALED_BIT)}

        vkDeviceWaitIdle(device.get)

    private def createIndexBuffer(): Unit = {
        indexCount = indices.length
        val bufferSize = indexCount * java.lang.Short.BYTES
        val data = BufferUtils.createByteBuffer(bufferSize)
        for (index <- indices){
            data.putShort(index)
        }
        data.rewind()

        val stagingBuffer = new Buffer(
            bufferSize,
            VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
            VMA_MEMORY_USAGE_CPU_ONLY,
            context.allocator
        )
        Buffer.copyBuffer(data, stagingBuffer, bufferSize)

        indexBuffer = new Buffer(
            bufferSize,
            VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
            VMA_MEMORY_USAGE_GPU_ONLY,
            context.allocator
        )

        val copyCmd = Buffer.copyBuffer(stagingBuffer, indexBuffer, bufferSize, commandPool)
        copyCmd.block()
        copyCmd.destroy()
        stagingBuffer.close()
    }

    private def createVertexBuffer(): Unit = {
        vertexCount = vertices.length
        val vertexData = Vertex.toByteBuffer(vertices)
        val bufferSize = vertexData.remaining()

        val stagingBuffer = new Buffer(
            bufferSize,
            VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
            VMA_MEMORY_USAGE_CPU_ONLY,
            context.allocator
        )
        Buffer.copyBuffer(vertexData, stagingBuffer, bufferSize)

        vertexBuffer = new Buffer(
            bufferSize,
            VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
            VMA_MEMORY_USAGE_GPU_ONLY,
            context.allocator
        )

        val copyCmd = Buffer.copyBuffer(stagingBuffer, vertexBuffer, bufferSize, commandPool)
        copyCmd.block()
        copyCmd.destroy()
        stagingBuffer.close()
    }

    private def destroySwapchainResources(): Unit =
        if swapchain != null then
            vkDeviceWaitIdle(device.get)
            Option(graphicsPipeline).foreach(_.destroy())
            graphicsPipeline = null
            Option(renderPass).foreach(_.destroyFramebuffers())
            Option(renderPass).foreach(_.destroy())
            renderPass = null

    def drawFrame(): Unit = pushStack: stack =>

        Option(inFlightFences(currentFrame)).foreach(_.block())

        val pImageIndex = stack.callocInt(1)
        val acquireResult = vkAcquireNextImageKHR(device.get, swapchain.get, Long.MaxValue, imageAvailableSemaphores(currentFrame).get, VK_NULL_HANDLE, pImageIndex)
        if (acquireResult == VK_ERROR_OUT_OF_DATE_KHR)
            recreateSwapchain()
            return
        else if (acquireResult != VK_SUCCESS && acquireResult != VK_SUBOPTIMAL_KHR)
            throw RuntimeException("failed to acquire swap chain image!")
        val imageIndex = pImageIndex.get(0)

        Option(inFlightFences(currentFrame)).foreach(_.reset())
        
        // Reset & record command buffer
        check(vkResetCommandBuffer(commandBuffers(currentFrame), 0), "Failed to reset command buffer")
        val framebuffer = renderPass.swapchainFramebuffers(imageIndex)
        if framebuffer == 0L then
          logger.warn(s"Framebuffer for imageIndex=$imageIndex is null")
          return
        val recordedOk = renderPass.recordCommandBuffer(
            commandBuffer = commandBuffers(currentFrame), 
            framebuffer = framebuffer, 
            imageIndex = imageIndex, 
            graphicsPipeline = graphicsPipeline, 
            vertexBuffer = vertexBuffer, 
            vertexCount = vertexCount, 
            indexedDraw = Some((indexBuffer, indexCount))
        )
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
        if (presentResult == VK_ERROR_OUT_OF_DATE_KHR || presentResult == VK_SUBOPTIMAL_KHR)
            recreateSwapchain()
        else if (presentResult != VK_SUCCESS)
            throw RuntimeException("failed to present swap chain image!");
        currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT

    def cleanup(): Unit =
        try
            vkDeviceWaitIdle(device.get)

            destroySwapchainResources()

            Option(vertexBuffer).foreach(_.close())
            Option(indexBuffer).foreach(_.close())

            if swapchain != null then
                swapchainManager.destroyImageViews(swapchain)
                swapchainManager.destroySwapchain(swapchain)
                swapchain = null

            imageAvailableSemaphores.foreach(s => Option(s).foreach(_.destroy()))
            renderFinishedSemaphores.foreach(s => Option(s).foreach(_.destroy()))
            inFlightFences.foreach(f => Option(f).foreach(_.destroy()))

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