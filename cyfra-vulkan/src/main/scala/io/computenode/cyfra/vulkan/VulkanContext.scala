package io.computenode.cyfra.vulkan

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.VulkanContext.ValidationLayers
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.core.{DebugCallback, Device, Instance, PhysicalDevice, Queue}
import io.computenode.cyfra.vulkan.memory.{Allocator, DescriptorPool}
import org.lwjgl.system.Configuration

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] object VulkanContext:
  val ValidationLayer: String = "VK_LAYER_KHRONOS_validation"
  private val ValidationLayers: Boolean = System.getProperty("io.computenode.cyfra.vulkan.validation", "false").toBoolean
  if Configuration.STACK_SIZE.get() < 100 then logger.warn(s"Small stack size. Increase with org.lwjgl.system.stackSize")

private[cyfra] class VulkanContext:
  private val instance: Instance = new Instance(ValidationLayers)
  private val debugCallback: Option[DebugCallback] = if ValidationLayers then Some(new DebugCallback(instance)) else None
  private val physicalDevice = new PhysicalDevice(instance)
  physicalDevice.assertRequirements()

  given device: Device = new Device(instance, physicalDevice)
  given allocator: Allocator = new Allocator(instance, physicalDevice, device)

  val queues = device.getQueues
  val descriptorPool: DescriptorPool = new DescriptorPool()
  val commandPool: CommandPool = new CommandPool.Standard(queues.head)

  logger.debug("Vulkan context created")
  logger.debug("Running on device: " + physicalDevice.name)

  def destroy(): Unit =
    commandPool.destroy()
    descriptorPool.destroy()
    queues.foreach(_.destroy())
    allocator.destroy()
    device.destroy()
    debugCallback.foreach(_.destroy())
    instance.destroy()
