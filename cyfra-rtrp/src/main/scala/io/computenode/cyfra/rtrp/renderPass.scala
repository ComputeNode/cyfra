package io.computenode.cyfra.rtrp

private[cyfra] class renderPass(context: VulkanContext) extends VulkanObjectHandle:

    private val device = conetxt.device
    protected val handle: Long = pushStack: stack =>
        val colorAttachment = VkAttachmentDescription 
            .calloc(stack)
            .format(/)
            .samples(VK_SAMPLE_COUNT_1_BIT)
            .loadOp(VK_ATTACHMENT_LOAD_OP_CLEAR)
            .storeOp(VK_ATTACHMENT_STORE_OP_STORE)
            .stencilLoadOp(VK_ATTACHMENT_LOAD_OP_DONT_CARE)
            .stencilStoreOp(VK_ATTACHMENT_STORE_OP_DONT_CARE)
            .initialLayout(VK_IMAGE_LAYOUT_UNDEFINED)
            .finalLayout(VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)
        
        val colorAttachmentRef = VkAttachmentReference 
            .calloc(stack)
            .attachment(0)
            .layout(VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
            
        val subpass = VkSubpassDescription 
            .calloc(stack)
            .pipelineBindPoint(VK_PIPELINE_BIND_POINT_GRAPHICS)
            .colorAttachmentCount(1)
            .pColorAttachments(colorAttachmentRef)
        
        val renderPassInfo = VkRenderPassCreateInfo 
            .calloc(stack)
            .sType$Default()
            .attachmentCount(1)
            .pAttachments(colorAttachment)
            .subpassCount(1)
            .pSubpasses(subpass)

        val pRenderPass = stack.callocLong(1)
        if (vkCreateRenderPass(device.get, renderPassInfo, null, renderPass) != VK_SUCCESS) then
            throw std::runtime_error("failed to create render pass!")
        pRenderPass.get(0)