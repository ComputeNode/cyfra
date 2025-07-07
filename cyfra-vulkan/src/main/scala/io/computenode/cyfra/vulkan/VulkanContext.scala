package io.computenode.cyfra.vulkan

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.VulkanContext.ValidationLayers
import io.computenode.cyfra.vulkan.command.{CommandPool}
import io.computenode.cyfra.vulkan.core.{DebugCallback, Device, Instance, Queue}
import io.computenode.cyfra.vulkan.memory.{Allocator, DescriptorPool}
import org.lwjgl.system.Configuration

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] object VulkanContext:
  val ValidationLayer: String = "VK_LAYER_KHRONOS_validation"
  val SyncLayer: String = "VK_LAYER_KHRONOS_synchronization2"
  private val ValidationLayers: Boolean = System.getProperty("io.computenode.cyfra.vulkan.validation", "false").toBoolean
  Option(Configuration.STACK_SIZE.get())
    .filter(_ < 100)
    .foreach(size => logger.warn(s"Stack size [$size] may fail during runtime. Set org.lwjgl.system.stackSize"))

private[cyfra] class VulkanContext:
  val instance: Instance = new Instance(ValidationLayers)
  val debugCallback: Option[DebugCallback] = if ValidationLayers then Some(new DebugCallback(instance)) else None
  given device: Device = new Device(instance)
  given allocator: Allocator = new Allocator(instance, device)
  val computeQueue: Queue = new Queue(device.computeQueueFamily, 0, device)
  val descriptorPool: DescriptorPool = new DescriptorPool()
  val commandPool: CommandPool = new CommandPool.Standard(computeQueue)

  logger.debug("Vulkan context created")
  logger.debug("Running on device: " + device.physicalDeviceName)

  def destroy(): Unit =
    commandPool.destroy()
    descriptorPool.destroy()
    computeQueue.destroy()
    allocator.destroy()
    device.destroy()
    debugCallback.foreach(_.destroy())
    instance.destroy()
