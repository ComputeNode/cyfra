package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.command.{CommandPool, Queue, StandardCommandPool}
import io.computenode.cyfra.vulkan.core.{DebugCallback, Device, Instance}
import io.computenode.cyfra.vulkan.memory.{Allocator, DescriptorPool}

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] object VulkanContext {
  final val ValidationLayer: String = "VK_LAYER_KHRONOS_validation"
  final val SyncLayer: String = "VK_LAYER_KHRONOS_synchronization2"
}

private[cyfra] class VulkanContext(val enableValidationLayers: Boolean = false) {
  private val sdkPresent = org.lwjgl.system.Configuration.VULKAN_LIBRARY_NAME.get() != null
  private val validationLayers = enableValidationLayers && sdkPresent

  val instance: Instance = new Instance(validationLayers)
  val debugCallback: DebugCallback = if (validationLayers) new DebugCallback(instance) else null
  val device: Device = new Device(instance)
  val computeQueue: Queue = new Queue(device.computeQueueFamily, 0, device)
  val allocator: Allocator = new Allocator(instance, device)
  val descriptorPool: DescriptorPool = new DescriptorPool(device)
  val commandPool: CommandPool = new StandardCommandPool(device, computeQueue)

  def destroy(): Unit = {
    commandPool.destroy()
    descriptorPool.destroy()
    allocator.destroy()
    computeQueue.destroy()
    device.destroy()
    if (validationLayers)
      debugCallback.destroy()
    instance.destroy()
  }
}
