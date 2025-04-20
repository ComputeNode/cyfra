package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.core.{Device, Instance}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.system.MemoryStack
import org.lwjgl.util.vma.Vma.{vmaCreateAllocator, vmaDestroyAllocator}
import org.lwjgl.util.vma.{VmaAllocatorCreateInfo, VmaVulkanFunctions}

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] class Allocator(instance: Instance, device: Device) extends VulkanObjectHandle {

  protected val handle: Long = pushStack { stack =>
    val functions = VmaVulkanFunctions.calloc(stack)
    functions.set(instance.get, device.get)
    val allocatorInfo = VmaAllocatorCreateInfo
      .calloc(stack)
      .device(device.get)
      .physicalDevice(device.physicalDevice)
      .instance(instance.get)
      .pVulkanFunctions(functions)

    val pAllocator = stack.callocPointer(1)
    check(vmaCreateAllocator(allocatorInfo, pAllocator), "Failed to create allocator")
    pAllocator.get(0)
  }

  def close(): Unit =
    vmaDestroyAllocator(handle)
}
