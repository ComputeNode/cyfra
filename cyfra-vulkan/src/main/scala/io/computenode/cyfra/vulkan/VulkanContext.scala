package io.computenode.cyfra.vulkan

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.VulkanContext.{validation, vulkanPrintf}
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.core.{DebugMessengerCallback, DebugReportCallback, Device, Instance, PhysicalDevice, Queue}
import io.computenode.cyfra.vulkan.memory.{Allocator, DescriptorPool, DescriptorPoolManager, DescriptorSetManager}
import org.lwjgl.system.Configuration

import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import scala.util.chaining.*
import scala.jdk.CollectionConverters.*

/** @author
  *   MarconZet Created 13.04.2020
  */
private[cyfra] object VulkanContext:
  private val validation: Boolean = System.getProperty("io.computenode.cyfra.vulkan.validation", "false").toBoolean
  private val vulkanPrintf: Boolean = System.getProperty("io.computenode.cyfra.vulkan.printf", "false").toBoolean

private[cyfra] class VulkanContext:
  private val instance: Instance = new Instance(validation, vulkanPrintf)
  private val debugReport: Option[DebugReportCallback] = if validation then Some(new DebugReportCallback(instance)) else None
  private val debugMessenger: Option[DebugMessengerCallback] = if validation & vulkanPrintf then Some(new DebugMessengerCallback(instance)) else None
  private val physicalDevice = new PhysicalDevice(instance)
  physicalDevice.assertRequirements()

  given device: Device = new Device(instance, physicalDevice)
  given allocator: Allocator = new Allocator(instance, physicalDevice, device)

  private val descriptorPoolManager = new DescriptorPoolManager()
  private val commandPools = device.getQueues.map(new CommandPool.Reset(_))

  logger.debug("Vulkan context created")
  logger.debug("Running on device: " + physicalDevice.name)

  private val blockingQueue: BlockingQueue[CommandPool.Reset] = new ArrayBlockingQueue(commandPools.length).tap(_.addAll(commandPools.asJava))
  def withThreadContext[T](f: VulkanThreadContext => T): T =
    assert(
      VulkanThreadContext.guard.get() == 0,
      "VulkanThreadContext is not thread-safe. Each thread can have only one VulkanThreadContext at a time. You cannot stack VulkanThreadContext.",
    )
    val commandPool = blockingQueue.take()
    val descriptorSetManager = new DescriptorSetManager(descriptorPoolManager)
    val threadContext = new VulkanThreadContext(commandPool, descriptorSetManager)
    VulkanThreadContext.guard.set(threadContext.hashCode())
    try f(threadContext)
    finally
      commandPool.reset()
      blockingQueue.put(commandPool)
      descriptorSetManager.destroy()
      VulkanThreadContext.guard.set(0)

  def destroy(): Unit =
    commandPools.foreach(_.destroy())
    descriptorPoolManager.destroy()
    allocator.destroy()
    device.destroy()
    debugReport.foreach(_.destroy())
    debugMessenger.foreach(_.destroy())
    instance.destroy()
