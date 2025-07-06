package io.computenode.cyfra.vulkan.archive

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.archive.VulkanContext.ValidationLayers
import io.computenode.cyfra.vulkan.archive.command.{CommandPool, Queue, StandardCommandPool}
import io.computenode.cyfra.vulkan.archive.core.{DebugCallback, Device, Instance}
import io.computenode.cyfra.vulkan.archive.memory.{Allocator, DescriptorPool}

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] object VulkanContext:
  val ValidationLayer: String = "VK_LAYER_KHRONOS_validation"
  val SyncLayer: String = "VK_LAYER_KHRONOS_synchronization2"
  private val ValidationLayers: Boolean = System.getProperty("io.computenode.cyfra.vulkan.validation", "false").toBoolean

private[cyfra] class VulkanContext:
  val instance: Instance = new Instance(ValidationLayers)
  val debugCallback: Option[DebugCallback] = if ValidationLayers then Some(new DebugCallback(instance)) else None
  val device: Device = new Device(instance)
  val computeQueue: Queue = new Queue(device.computeQueueFamily, 0, device)
  val allocator: Allocator = new Allocator(instance, device)
  val descriptorPool: DescriptorPool = new DescriptorPool(device)
  val commandPool: CommandPool = new StandardCommandPool(device, computeQueue)

  logger.debug("Vulkan context created")
  logger.debug("Running on device: " + device.physicalDeviceName)

  def destroy(): Unit =
    commandPool.destroy()
    descriptorPool.destroy()
    allocator.destroy()
    computeQueue.destroy()
    device.destroy()
    debugCallback.foreach(_.destroy())
    instance.destroy()
