package io.computenode.cyfra.rtrp

import org.lwjgl.vulkan.VK10.*
import io.computenode.cyfra.rtrp.*
import io.computenode.cyfra.rtrp.graphics.*
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.vulkan.KHRSwapchain.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
import org.lwjgl.vulkan.*
import io.computenode.cyfra.vulkan.memory.Buffer
import io.computenode.cyfra.vulkan.memory.DescriptorSet
import java.nio.ByteBuffer


private[cyfra] class RenderPass(context: VulkanContext, swapchain: Swapchain) extends VulkanObjectHandle:

    private val device = context.device
    protected val handle: Long = pushStack: stack =>
        val colorAttachment = VkAttachmentDescription 
            .calloc(1, stack)
            .format(swapchain.format)
            .samples(VK_SAMPLE_COUNT_1_BIT)
            .loadOp(VK_ATTACHMENT_LOAD_OP_CLEAR)
            .storeOp(VK_ATTACHMENT_STORE_OP_STORE)
            .stencilLoadOp(VK_ATTACHMENT_LOAD_OP_DONT_CARE)
            .stencilStoreOp(VK_ATTACHMENT_STORE_OP_DONT_CARE)
            .initialLayout(VK_IMAGE_LAYOUT_UNDEFINED)
            .finalLayout(VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)
        
        val colorAttachmentRef = VkAttachmentReference 
            .calloc(1, stack)
            .attachment(0)
            .layout(VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
            
        val subpass = VkSubpassDescription 
            .calloc(1, stack)
            .pipelineBindPoint(VK_PIPELINE_BIND_POINT_GRAPHICS)
            .colorAttachmentCount(1)
            .pColorAttachments(colorAttachmentRef)
        
        val dependency = VkSubpassDependency
            .calloc(1, stack)
            .srcSubpass(VK_SUBPASS_EXTERNAL)
            .dstSubpass(0)
            .srcStageMask(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
            .srcAccessMask(0)
            .dstStageMask(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
            .dstAccessMask(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
        
        val renderPassInfo = VkRenderPassCreateInfo 
            .calloc(stack)
            .sType$Default()
            .pAttachments(colorAttachment)
            .pSubpasses(subpass)
            .pDependencies(dependency)

        val pRenderPass = stack.callocLong(1)
        if (vkCreateRenderPass(device.get, renderPassInfo, null, pRenderPass) != VK_SUCCESS) then
            throw new RuntimeException("failed to create render pass!")
        pRenderPass.get(0)

    private val renderPass = handle
    
    val swapchainFramebuffers = SwapchainManager.createFramebuffers(swapchain, renderPass)

    def recordCommandBuffer(
        commandBuffer: VkCommandBuffer,
        framebuffer: Long,
        imageIndex: Int,
        graphicsPipeline: GraphicsPipeline,
        vertexBuffer: Buffer,
        vertexCount: Int,
        indexedDraw: Option[(Buffer, Int)] = None,
        descriptorSet: Option[DescriptorSet] = None,
        pushConstants: Option[ByteBuffer] = None
    ): Boolean = pushStack: stack =>
        var finished = false
        try
            val beginInfo = VkCommandBufferBeginInfo 
                .calloc(stack)
                .sType$Default()

            check(vkBeginCommandBuffer(commandBuffer, beginInfo), "failed to begin recording command buffer!")

            val clearValues = VkClearValue.calloc(1, stack)
            clearValues.color().float32(0, 0f).float32(1, 0f).float32(2, 0f).float32(3, 1f)

            val renderArea = VkRect2D.calloc(stack)
            renderArea.offset().set(0, 0)
            renderArea.extent().set(swapchain.width, swapchain.height)

            val renderPassInfo = VkRenderPassBeginInfo 
                .calloc(stack)
                .sType$Default()
                .renderPass(renderPass)
                .framebuffer(framebuffer)
                .renderArea(renderArea)
                .pClearValues(clearValues)

            vkCmdBeginRenderPass(commandBuffer, renderPassInfo, VK_SUBPASS_CONTENTS_INLINE)
            
            vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, graphicsPipeline.get)

            descriptorSet.foreach { ds =>
                vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, graphicsPipeline.layout, 0, stack.longs(ds.get), null)
            }

            pushConstants.foreach { pc =>
                vkCmdPushConstants(commandBuffer, graphicsPipeline.layout, VK_SHADER_STAGE_FRAGMENT_BIT, 0, pc)
            }
            
            // val viewport = VkViewport 
            //     .calloc(1, stack)
            //     .x(0.0f)
            //     .y(0.0f)
            //     .width(swapchain.extent.width().toFloat)
            //     .height(swapchain.extent.height().toFloat)
            //     .minDepth(0.0f)
            //     .maxDepth(1.0f)
            // vkCmdSetViewport(commandBuffer, 0, viewport)
            // val scissor = VkRect2D 
            //     .calloc(1, stack)
            //     .offset(VkOffset2D.calloc(stack).set(0,0))
            //     .extent(swapchain.extent)
            // vkCmdSetScissor(commandBuffer, 0, scissor)

            val pBuffers = stack.longs(vertexBuffer.get)
            val pOffsets = stack.longs(0L)
            vkCmdBindVertexBuffers(commandBuffer, 0, pBuffers, pOffsets)

            indexedDraw match {
                case Some((indexBuffer, indexCount)) =>
                    vkCmdBindIndexBuffer(commandBuffer, indexBuffer.get, 0, VK_INDEX_TYPE_UINT16)
                    vkCmdDrawIndexed(commandBuffer, indexCount, 1, 0, 0, 0)
                case None =>
                    vkCmdDraw(commandBuffer, vertexCount, 1, 0, 0) 
            }

            vkCmdEndRenderPass(commandBuffer)

            check(vkEndCommandBuffer(commandBuffer), "failed to end command buffer")
            finished = true
            true
        catch 
            case t: Throwable =>
                if !finished then
                    try vkEndCommandBuffer(commandBuffer) catch case _: Throwable => ()
                false

    def destroyFramebuffers(): Unit =
        if swapchainFramebuffers != null then
            for fb <- swapchainFramebuffers do
                if fb != 0L then vkDestroyFramebuffer(device.get, fb, null)

    override def close(): Unit = 
        vkDestroyRenderPass(device.get, renderPass, null)
        alive = false