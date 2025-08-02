package io.computenode.cyfra.vulkan

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.VulkanContext.ValidationLayers
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.core.{DebugCallback, Device, Instance, PhysicalDevice, Queue}
import io.computenode.cyfra.vulkan.memory.{Allocator, DescriptorPool, DescriptorPoolManager}
import org.lwjgl.system.Configuration

import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import scala.util.chaining.*
import scala.jdk.CollectionConverters.*

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

  private val descriptorPoolManager = new DescriptorPoolManager()
  private val commandPools = device.getQueues.map(new CommandPool.Transient(_))

  logger.debug("Vulkan context created")
  logger.debug("Running on device: " + physicalDevice.name)

  private val blockingQueue: BlockingQueue[CommandPool] = new ArrayBlockingQueue[CommandPool](commandPools.length).tap(_.addAll(commandPools.asJava))
  def withThreadContext[T](f: VulkanThreadContext => T): T =
    assert(
      VulkanThreadContext.guard.get() == 0,
      "VulkanThreadContext is not thread-safe. Each thread can have only one VulkanThreadContext at a time. You cannot stack VulkanThreadContext.",
    )
    val commandPool = blockingQueue.take()
    val threadContext = new VulkanThreadContext(commandPool, descriptorPoolManager)
    VulkanThreadContext.guard.set(threadContext.hashCode())
    try f(threadContext)
    finally
      blockingQueue.put(commandPool)
      VulkanThreadContext.guard.set(0)

  def destroy(): Unit =
    commandPools.foreach(_.destroy())
    descriptorPoolManager.destroy()
    allocator.destroy()
    device.destroy()
    debugCallback.foreach(_.destroy())
    instance.destroy()
