package io.computenode.cyfra.vulkan

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.VulkanContext.ValidationLayers
import io.computenode.cyfra.vulkan.command.*
import io.computenode.cyfra.vulkan.core.*
import io.computenode.cyfra.vulkan.memory.*

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] object VulkanContext:
  val ValidationLayer: String = "VK_LAYER_KHRONOS_validation"
  val SyncLayer: String = "VK_LAYER_KHRONOS_synchronization2"
  private val ValidationLayers: Boolean = System.getProperty("io.computenode.cyfra.vulkan.validation", "false").toBoolean

  def apply(): VulkanContext = new VulkanContext(enableSurfaceExtensions = false)

  def withSurfaceSupport(): VulkanContext = new VulkanContext(enableSurfaceExtensions = true)

private[cyfra] class VulkanContext(enableSurfaceExtensions: Boolean = false):

  val instance: Instance = new Instance(ValidationLayers, enableSurfaceExtensions)
  val debugCallback: Option[DebugCallback] = if ValidationLayers then Some(new DebugCallback(instance)) else None
  val device: Device = new Device(instance)
  val computeQueue: Queue = new Queue(device.computeQueueFamily, 0, device)
  val allocator: Allocator = new Allocator(instance, device)
  val descriptorPool: DescriptorPool = new DescriptorPool(device)
  val commandPool: CommandPool = new StandardCommandPool(device, computeQueue)

  if enableSurfaceExtensions then logger.debug("Vulkan context created with surface extension support")
  else logger.debug("Vulkan context created (compute-only)")
  logger.debug("Running on device: " + device.physicalDeviceName)

  def destroy(): Unit =
    commandPool.destroy()
    descriptorPool.destroy()
    allocator.destroy()
    computeQueue.destroy()
    device.destroy()
    debugCallback.foreach(_.destroy())
    instance.destroy()
