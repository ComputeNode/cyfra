package io.computenode.cyfra.vulkan.pipeline.graphics

import io.computenode.cyfra.vulkan.pipeline.LayoutInfo
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.VulkanObjectHandle
import io.computenode.cyfra.vulkan.device.Device
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.*

private[cyfra] class GraphicsPipeline (vertShader: Shader, fragShader: Shader, context: VulkanContext) extends VulkanObjectHandle:

    private val device: Device = context.device

    protected val handle: Long = pushStack: stack =>
        val shaderStages = VkPipelineShaderStageCreateInfo.calloc(2, stack)

        val vertStageInfo = shaderStages(0)
        vertStage
            .calloc(stack)
            .sType$Default()
            .stage(VK_SHADER_STAGE_VERTEX_BIT)
            .module(vertShader.get)
            .pName(vertShader.functionName)
        
        val fragStageInfo = shaderStages(1)
        fragStage
            .calloc(stack)
            .sType$Default()
            .stage(VK_SHADER_STAGE_FRAGMENT_BIT)
            .module(fragShader.get)
            .pName(fragShader.functionName)

        
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
            .depthClampEnable(VK_FALSE)
            .rasterizerDiscardEnable(VK_FALSE)
            .polygonMode(VK_POLYGON_MODE_FILL)
            .lineWidth(1.0f)
            .cullMode(VK_CULL_MODE_BACK_BIT)
            .frontFace(VK_FRONT_FACE_CLOCKWISE)
            .depthBiasEnable(VK_FALSE)
            .depthBiasConstantFactor(0.0f)      // Optional
            .depthBiasClamp(0.0f)               // Optional
            .depthBiasSlopeFactor(0.0f)         // Optional

        val multisampling = VkPipelineMultisampleStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .sampleShadingEnable(VK_FALSE)
            .rasterizationSamples(VK_SAMPLE_COUNT_1_BIT)
            .minSampleShading(1.0f) // Optional
            .pSampleMask(null) // Optional
            .alphaToCoverageEnable(VK_FALSE) // Optional
            .alphaToOneEnable(VK_FALSE) // Optional            

        val colorBlendAttachment = VkPipelineColorBlendAttachmentState 
            .calloc(stack)
            .colorWriteMask(VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT)
            .blendEnable(VK_FALSE)
            .srcColorBlendFactor(VK_BLEND_FACTOR_ONE) // Optional
            .dstColorBlendFactor(VK_BLEND_FACTOR_ZERO) // Optional
            .colorBlendOp(VK_BLEND_OP_ADD) // Optional
            .srcAlphaBlendFactor(VK_BLEND_FACTOR_ONE) // Optional
            .dstAlphaBlendFactor(VK_BLEND_FACTOR_ZERO) // Optional
            .alphaBlendOp(VK_BLEND_OP_ADD) // Optional

        val colorBlending = VkPipelineColorBlendStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .logicOpEnable(VK_FALSE)
            .logicOp(VK_LOGIC_OP_COPY) // Optional
            .attachmentCount(1)
            .pAttachments(colorBlendAttachment)
            .blendConstants[0](0.0f) // Optional
            .blendConstants[1](0.0f) // Optional
            .blendConstants[2](0.0f) // Optional
            .blendConstants[3](0.0f) // Optional            
        
        val dynamicStates = stack.ints(
            VK_DYNAMIC_STATE_VIEWPORT,
            VK_DYNAMIC_STATE_SCISSOR
        )

        val dynamicState = VkPipelineDynamicStateCreateInfo
            .calloc(stack)
            .sType$Default()
            .dynamicStateCount(dynamicStates.capacity())
            .pDynamicStates(dynamicStates)

        val inputAssembly = VkPipelineInputAssemblyStateCreateInfo 
            .calloc(stack)
            .sType$Default()
            .topology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
            .primitiveRestartEnable(VK_FALSE)

        val viewport = VkViewport 
            .calloc(stack)
            .x(0.0f)
            .y(0.0f)
            .width(/)
            .height(/)
            .minDepth(0.0f)
            .maxDepth(1.0f)
        
        val scissor = VkRect2D
            .calloc(stack)
            .offset(0,0)
            .extent(/)

        val pipelineLayout: VkPipelineLayout  =
            val pipelineLayoutInfo = VkPipelineLayoutCreateInfo.calloc(stack)
            pipelineLayoutInfo
                .sType$Default()
                .setLayoutCount(0) // Optional
                .pSetLayouts(null) // Optional
                .pushConstantRangeCount(0) // Optional
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
            .renderPass(/)
            .subpass(0)
            .basePipelineHandle(VK_NULL_HANDLE) // Optional
            .basePipelineIndex(-1)  // Optional
        
        val pGraphicsPipeline = stack.callocLong(1, stack)
        if (vkCreateGraphicsPipelines(device.get, VK_NULL_HANDLE, 1, pipelineInfo, null, pGraphicsPipeline) != VK_SUCCESS) then
            throw std::runtime_error("failed to create graphics pipeline!")
        pGraphicsPipeline.get(0)
