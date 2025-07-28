package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK11.vkGetPhysicalDeviceFeatures2
import org.lwjgl.vulkan.*

import java.nio.ByteBuffer
import scala.jdk.CollectionConverters.given

class PhysicalDevice(instance: Instance) extends VulkanObject[VkPhysicalDevice] {
  protected val handle: VkPhysicalDevice = pushStack: stack =>
    val pPhysicalDeviceCount = stack.callocInt(1)
    check(vkEnumeratePhysicalDevices(instance.get, pPhysicalDeviceCount, null), "Failed to get number of physical devices")
    val deviceCount = pPhysicalDeviceCount.get(0)
    if deviceCount == 0 then throw new AssertionError("Failed to find GPUs with Vulkan support")
    val pPhysicalDevices = stack.callocPointer(deviceCount)
    check(vkEnumeratePhysicalDevices(instance.get, pPhysicalDeviceCount, pPhysicalDevices), "Failed to get physical devices")
    new VkPhysicalDevice(pPhysicalDevices.get(), instance.get)

  override protected def close(): Unit = {}

  private val pdp: VkPhysicalDeviceProperties =
    val pProperties = VkPhysicalDeviceProperties.create()
    vkGetPhysicalDeviceProperties(handle, pProperties)
    pProperties

  private val (pdf, v11f, v12f, v13f)
    : (VkPhysicalDeviceFeatures, VkPhysicalDeviceVulkan11Features, VkPhysicalDeviceVulkan12Features, VkPhysicalDeviceVulkan13Features) =
    val vulkan11Features = VkPhysicalDeviceVulkan11Features.create().sType$Default()
    val vulkan12Features = VkPhysicalDeviceVulkan12Features.create().sType$Default()
    val vulkan13Features = VkPhysicalDeviceVulkan13Features.create().sType$Default()

    val physicalDeviceFeatures = VkPhysicalDeviceFeatures2
      .create()
      .sType$Default()
      .pNext(vulkan11Features)
      .pNext(vulkan12Features)
      .pNext(vulkan13Features)

    vkGetPhysicalDeviceFeatures2(handle, physicalDeviceFeatures)
    val features = VkPhysicalDeviceFeatures.create().set(physicalDeviceFeatures.features())
    (features, vulkan11Features, vulkan12Features, vulkan13Features)

  private val extensionProperties = pushStack: stack =>
    val pPropertiesCount = stack.callocInt(1)
    check(
      vkEnumerateDeviceExtensionProperties(handle, null.asInstanceOf[ByteBuffer], pPropertiesCount, null),
      "Failed to get number of properties extension",
    )
    val propertiesCount = pPropertiesCount.get(0)

    val pProperties = VkExtensionProperties.create(propertiesCount)
    check(
      vkEnumerateDeviceExtensionProperties(handle, null.asInstanceOf[ByteBuffer], pPropertiesCount, pProperties),
      "Failed to get extension properties",
    )
    pProperties

  def assertRequirements(): Unit =
    assert(v13f.synchronization2(), "Vulkan 1.3 synchronization2 feature is required")

  def name: String = pdp.deviceNameString()
  def deviceExtensionsSet: Set[String] = extensionProperties.iterator().asScala.map(_.extensionNameString()).toSet

  def selectComputeQueueFamily: (Int, Int) = pushStack: stack =>
    val pQueueFamilyCount = stack.callocInt(1)
    vkGetPhysicalDeviceQueueFamilyProperties(handle, pQueueFamilyCount, null)
    val queueFamilyCount = pQueueFamilyCount.get(0)

    val pQueueFamilies = VkQueueFamilyProperties.calloc(queueFamilyCount, stack)
    vkGetPhysicalDeviceQueueFamilyProperties(handle, pQueueFamilyCount, pQueueFamilies)

    val queues = pQueueFamilies.iterator().asScala.map(_.queueFlags()).zipWithIndex.toSeq
    val onlyCompute = queues.find: (flags, _) =>
      ~(VK_QUEUE_GRAPHICS_BIT & flags) > 0 && (VK_QUEUE_COMPUTE_BIT & flags) > 0
    val hasCompute = queues.find: (flags, _) =>
      (VK_QUEUE_COMPUTE_BIT & flags) > 0

    val (_, index) = onlyCompute
      .orElse(hasCompute)
      .getOrElse(throw new AssertionError("No suitable queue family found for computing"))

    (index, pQueueFamilies.get(index).queueCount())
}
