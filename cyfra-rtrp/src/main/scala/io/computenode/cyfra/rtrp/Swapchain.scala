package io.computenode.cyfra.rtrp

import org.lwjgl.vulkan.VkExtent2D

class Swapchain(
    handle: Long,
    images: Array[Long],
    imageViews: Array[Long],
    format: Int,
    colorSpace: Int,
    extent: VkExtent2D
)