package io.computenode.cyfra.rtrp

import org.lwjgl.vulkan.VK10.*
import io.computenode.cyfra.rtrp.*
import io.computenode.cyfra.rtrp.graphics.*
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.vulkan.KHRSwapchain.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
import org.lwjgl.vulkan.*


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

    def recordCommandBuffer(commandBuffer: VkCommandBuffer, imageIndex: Int, graphicsPipeline: Long): Unit = pushStack: stack =>
        val beginInfo = VkCommandBufferBeginInfo 
            .calloc(stack)
            .sType$Default
            .flags(0) // Optional
            .pInheritanceInfo(null)
        
        if (vkBeginCommandBuffer(commandBuffer, beginInfo) != VK_SUCCESS) then
            throw new RuntimeException("failed to begin recording command buffer!")
        
        val renderArea = VkRect2D.calloc(stack)
            .offset(VkOffset2D.calloc(stack).set(0, 0))
            .extent(swapchain.extent)

        val renderPassInfo = VkRenderPassBeginInfo 
            .calloc(stack)
            .sType$Default
            .renderPass(renderPass)
            .framebuffer(swapchainFramebuffers(imageIndex))
            .renderArea(renderArea)

        val clearColor = VkClearValue.calloc(1, stack)
        clearColor.get(0).color().float32(stack.floats(0.0f, 0.0f, 0.0f, 1.0f))

        renderPassInfo
            .clearValueCount(1)
            .pClearValues(clearColor)

        vkCmdBeginRenderPass(commandBuffer, renderPassInfo, VK_SUBPASS_CONTENTS_INLINE)
            vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, graphicsPipeline)
            val viewport = VkViewport 
                .calloc(1, stack)
                .x(0.0f)
                .y(0.0f)
                .width(swapchain.extent.width().toFloat)
                .height(swapchain.extent.height().toFloat)
                .minDepth(0.0f)
                .maxDepth(1.0f)
            vkCmdSetViewport(commandBuffer, 0, viewport)

            val scissor = VkRect2D 
                .calloc(1, stack)
                .offset(VkOffset2D.calloc(stack).set(0,0))
                .extent(swapchain.extent)
            vkCmdSetScissor(commandBuffer, 0, scissor)

            vkCmdDraw(commandBuffer, 3, 1, 0, 0)
        
        vkCmdEndRenderPass(commandBuffer)

    override def close(): Unit = 
        vkDestroyRenderPass(device.get, renderPass, null)
        alive = false