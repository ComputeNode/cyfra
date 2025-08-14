package io.computenode.cyfra.rtrp

import org.lwjgl.vulkan.VK10.*
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
        
        val renderPassInfo = VkRenderPassCreateInfo 
            .calloc(stack)
            .sType$Default()
            .pAttachments(colorAttachment)
            .pSubpasses(subpass)

        val pRenderPass = stack.callocLong(1)
        if (vkCreateRenderPass(device.get, renderPassInfo, null, pRenderPass) != VK_SUCCESS) then
            throw new RuntimeException("failed to create render pass!")
        pRenderPass.get(0)