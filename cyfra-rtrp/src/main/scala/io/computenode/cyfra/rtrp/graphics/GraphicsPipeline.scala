package io.computenode.cyfra.rtrp.graphics

import io.computenode.cyfra.vulkan.compute.LayoutInfo
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.rtrp.{RenderPass, Swapchain}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.*

private[cyfra] class GraphicsPipeline (swapchain: Swapchain, vertShader: Shader, fragShader: Shader, context: VulkanContext, renderPass: RenderPass) extends VulkanObjectHandle:

    private val device: Device = context.device

    protected val handle: Long = pushStack: stack =>
        val shaderStages = VkPipelineShaderStageCreateInfo.calloc(2, stack)

        val vertStageInfo = shaderStages.get(0)
        vertStageInfo
            .sType$Default()
            .stage(VK_SHADER_STAGE_VERTEX_BIT)
            .module(vertShader.get)
            .pName(MemoryUtil.memUTF8(vertShader.functionName))
        
        val fragStageInfo = shaderStages.get(1)
        fragStageInfo
            .sType$Default()
            .stage(VK_SHADER_STAGE_FRAGMENT_BIT)
            .module(fragShader.get)
            .pName(MemoryUtil.memUTF8(fragShader.functionName))

        
        // val vertexInputInfo = VkPipelineVertexInputStateCreateInfo 
        //     .calloc(stack)
        //     .sType$Default()
        //     .vertexBindingDescriptionCount(0)
        //     .pVertexBindingDescriptions(null) // Optional
        //     .vertexAttributeDescriptionCount(0)
        //     .pVertexAttributeDescriptions(null) // Optional

        val viewportState = VkPipelineViewportStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .viewportCount(1)
            .scissorCount(1)
        
        val rasterizer = VkPipelineRasterizationStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .depthClampEnable(false)
            .rasterizerDiscardEnable(false)
            .polygonMode(VK_POLYGON_MODE_FILL)
            .lineWidth(1.0f)
            .cullMode(VK_CULL_MODE_BACK_BIT)
            .frontFace(VK_FRONT_FACE_CLOCKWISE)
            .depthBiasEnable(false)
            .depthBiasConstantFactor(0.0f)      // Optional
            .depthBiasClamp(0.0f)               // Optional
            .depthBiasSlopeFactor(0.0f)         // Optional

        val multisampling = VkPipelineMultisampleStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .sampleShadingEnable(false)
            .rasterizationSamples(VK_SAMPLE_COUNT_1_BIT)
            .minSampleShading(1.0f) // Optional
            .pSampleMask(null) // Optional
            .alphaToCoverageEnable(false) // Optional
            .alphaToOneEnable(false) // Optional            

        val colorBlendAttachment = VkPipelineColorBlendAttachmentState 
            .calloc(1, stack)
            .colorWriteMask(VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT)
            .blendEnable(false)
            .srcColorBlendFactor(VK_BLEND_FACTOR_ONE) // Optional
            .dstColorBlendFactor(VK_BLEND_FACTOR_ZERO) // Optional
            .colorBlendOp(VK_BLEND_OP_ADD) // Optional
            .srcAlphaBlendFactor(VK_BLEND_FACTOR_ONE) // Optional
            .dstAlphaBlendFactor(VK_BLEND_FACTOR_ZERO) // Optional
            .alphaBlendOp(VK_BLEND_OP_ADD) // Optional

        val colorBlending = VkPipelineColorBlendStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .logicOpEnable(false)
            .logicOp(VK_LOGIC_OP_COPY) // Optional
            .attachmentCount(1)
            .pAttachments(colorBlendAttachment)
            .blendConstants(stack.floats(0.0f, 0.0f, 0.0f, 0.0f))          
        
        val dynamicStates = stack.ints(
            VK_DYNAMIC_STATE_VIEWPORT,
            VK_DYNAMIC_STATE_SCISSOR
        )

        val dynamicState = VkPipelineDynamicStateCreateInfo
            .calloc(stack)
            .sType$Default()
            .pDynamicStates(dynamicStates)

        val inputAssembly = VkPipelineInputAssemblyStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .topology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
            .primitiveRestartEnable(false)

        val viewport = VkViewport 
            .calloc(stack)
            .x(0.0f)
            .y(0.0f)
            .width(swapchain.extent.width().toFloat)
            .height(swapchain.extent.height().toFloat)
            .minDepth(0.0f)
            .maxDepth(1.0f)
        
        val scissor = VkRect2D
            .calloc(stack)
            .offset(VkOffset2D.calloc(stack).set(0, 0))
            .extent(swapchain.extent)

        val pipelineLayout: Long  =
            val pipelineLayoutInfo = VkPipelineLayoutCreateInfo
                .calloc(stack)
                .sType$Default()
                .setLayoutCount(0) // Optional
                .pSetLayouts(null) // Optional
                .pPushConstantRanges(null) // Optional
            val pPipelineLayout = stack.mallocLong(1)
            if (vkCreatePipelineLayout(device.get, pipelineLayoutInfo, null, pPipelineLayout) != VK_SUCCESS) then
                throw new RuntimeException("Failed to create pipeline layout")
            pPipelineLayout.get(0)

        val pipelineInfo = VkGraphicsPipelineCreateInfo 
            .calloc(stack)
            .sType$Default()
            .stageCount(2)
            .pStages(shaderStages)            
            .pVertexInputState(vertexInputInfo)
            .pInputAssemblyState(inputAssembly)
            .pViewportState(viewportState)
            .pRasterizationState(rasterizer)
            .pMultisampleState(multisampling)
            .pDepthStencilState(null) // Optional
            .pColorBlendState(colorBlending)
            .pDynamicState(dynamicState)
            .layout(pipelineLayout)
            .renderPass(renderPass.get)
            .subpass(0)
            .basePipelineHandle(VK_NULL_HANDLE) // Optional
            .basePipelineIndex(-1)  // Optional
        
        val pGraphicsPipeline = stack.callocLong(1)
        if (vkCreateGraphicsPipelines(device.get, VK_NULL_HANDLE, 1, pipelineInfo, null, pGraphicsPipeline) != VK_SUCCESS) then
            throw new RuntimeException("failed to create graphics pipeline!")
        pGraphicsPipeline.get(0)
