package io.computenode.cyfra.rtrp.graphics

import io.computenode.cyfra.vulkan.compute.LayoutInfo
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.rtrp.{RenderPass, Swapchain}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.rtrp.Vertex
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.*

private[cyfra] class GraphicsPipeline(swapchain: Swapchain, vertShader: Shader, fragShader: Shader, context: VulkanContext, renderPass: RenderPass)
    extends VulkanObjectHandle:

  private val device: Device = context.device

  val (handle, layout, descriptorSetLayout) = pushStack: stack =>
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

    val bindingDescription = VkVertexInputBindingDescription
      .calloc(1, stack)
      .stride(Vertex.SIZEOF)
      .inputRate(VK_VERTEX_INPUT_RATE_VERTEX)

    val attributeDescriptions = VkVertexInputAttributeDescription.calloc(2, stack)

    // position
    attributeDescriptions
      .get(0)
      .binding(0)
      .location(0)
      .format(VK_FORMAT_R32G32_SFLOAT)
      .offset(Vertex.OFFSETOF_POS)
    // color
    attributeDescriptions
      .get(1)
      .binding(0)
      .location(1)
      .format(VK_FORMAT_R32G32B32_SFLOAT)
      .offset(Vertex.OFFSETOF_COLOR)

    val vertexInputInfo = VkPipelineVertexInputStateCreateInfo
      .calloc(stack)
      .sType$Default()
      .pVertexBindingDescriptions(bindingDescription)
      .pVertexAttributeDescriptions(attributeDescriptions)

    val viewport = VkViewport
      .calloc(1, stack)
      .x(0.0f)
      .y(0.0f)
      .width(swapchain.width.toFloat)
      .height(swapchain.height.toFloat)
      .minDepth(0.0f)
      .maxDepth(1.0f)

    val scissor = VkRect2D
      .calloc(1, stack)
      .offset(VkOffset2D.calloc(stack).set(0, 0))
      .extent(VkExtent2D.calloc(stack).width(swapchain.width).height(swapchain.height))

    val viewportState = VkPipelineViewportStateCreateInfo
      .calloc(stack)
      .sType$Default()
      .viewportCount(1)
      .scissorCount(1)
      .pViewports(viewport)
      .pScissors(scissor)

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
      .depthBiasConstantFactor(0.0f) // Optional
      .depthBiasClamp(0.0f) // Optional
      .depthBiasSlopeFactor(0.0f) // Optional

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

    val dslBinding = VkDescriptorSetLayoutBinding
      .calloc(1, stack)
      .binding(0)
      .descriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
      .descriptorCount(1)
      .stageFlags(VK_SHADER_STAGE_FRAGMENT_BIT)

    val dslCreateInfo = VkDescriptorSetLayoutCreateInfo
      .calloc(stack)
      .sType$Default()
      .pBindings(dslBinding)

    val pDescriptorSetLayout = stack.callocLong(1)
    check(vkCreateDescriptorSetLayout(device.get, dslCreateInfo, null, pDescriptorSetLayout), "failed to create descriptor set layout")
    val descriptorSetLayout = pDescriptorSetLayout.get(0)

    val pPushConstantRange = VkPushConstantRange
      .calloc(1, stack)
      .stageFlags(VK_SHADER_STAGE_FRAGMENT_BIT)
      .offset(0)
      .size(8) // size of 2 ints (width + useAlpha)

    val pipelineLayoutInfo = VkPipelineLayoutCreateInfo
      .calloc(stack)
      .sType$Default()
      .pSetLayouts(stack.longs(descriptorSetLayout))
      .pPushConstantRanges(pPushConstantRange)

    val pPipelineLayout = stack.callocLong(1)
    check(vkCreatePipelineLayout(device.get, pipelineLayoutInfo, null, pPipelineLayout), "Failed to create pipeline layout")
    val pipelineLayout = pPipelineLayout.get(0)

    // val dynamicStates = stack.ints(
    //     VK_DYNAMIC_STATE_VIEWPORT,
    //     VK_DYNAMIC_STATE_SCISSOR
    // )

    // val dynamicState = VkPipelineDynamicStateCreateInfo
    //     .calloc(stack)
    //     .sType$Default()
    //     .pDynamicStates(dynamicStates)

    val inputAssembly = VkPipelineInputAssemblyStateCreateInfo
      .calloc(stack)
      .sType$Default()
      .topology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
      .primitiveRestartEnable(false)

    val pipelineInfo = VkGraphicsPipelineCreateInfo
      .calloc(1, stack)
      .sType$Default()
      .pStages(shaderStages)
      .pVertexInputState(vertexInputInfo)
      .pInputAssemblyState(inputAssembly)
      .pViewportState(viewportState)
      .pRasterizationState(rasterizer)
      .pMultisampleState(multisampling)
      .pDepthStencilState(null) // Optional
      .pColorBlendState(colorBlending)
      // .pDynamicState(dynamicState)
      .layout(pipelineLayout)
      .renderPass(renderPass.get)
      .subpass(0)

    val pGraphicsPipeline = stack.callocLong(1)
    check(vkCreateGraphicsPipelines(device.get, VK_NULL_HANDLE, pipelineInfo, null, pGraphicsPipeline), "Failed to create graphics pipeline")
    (pGraphicsPipeline.get(0), pipelineLayout, descriptorSetLayout)

  private val graphicsPipeline = handle

  override def close(): Unit =
    vkDestroyDescriptorSetLayout(device.get, descriptorSetLayout, null)
    vkDestroyPipelineLayout(device.get, layout, null)
    vkDestroyPipeline(device.get, handle, null)
    alive = false
