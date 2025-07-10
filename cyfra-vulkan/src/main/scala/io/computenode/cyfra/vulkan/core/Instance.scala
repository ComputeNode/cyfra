package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.VulkanContext.ValidationLayer
import io.computenode.cyfra.vulkan.util.Util.*
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.glfw.GLFWVulkan
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.EXTDebugReport.VK_EXT_DEBUG_REPORT_EXTENSION_NAME
import org.lwjgl.vulkan.KHRPortabilityEnumeration.*
import org.lwjgl.vulkan.VK10.*

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.util.chaining.*

/** @author
  *   MarconZet Created 13.04.2020
  */
object Instance:
  val ValidationLayersExtensions: Seq[String] = List(VK_EXT_DEBUG_REPORT_EXTENSION_NAME)
  val MoltenVkExtensions: Seq[String] = List(VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME)

  lazy val (extensions, layers): (Seq[String], Seq[String]) = pushStack: stack =>
    val ip = stack.ints(1)
    vkEnumerateInstanceLayerProperties(ip, null)
    val availableLayers = VkLayerProperties.malloc(ip.get(0), stack)
    vkEnumerateInstanceLayerProperties(ip, availableLayers)

    vkEnumerateInstanceExtensionProperties(null.asInstanceOf[String], ip, null)
    val instance_extensions = VkExtensionProperties.malloc(ip.get(0), stack)
    vkEnumerateInstanceExtensionProperties(null.asInstanceOf[String], ip, instance_extensions)

    val extensions = instance_extensions.iterator().asScala.map(_.extensionNameString())
    val layers = availableLayers.iterator().asScala.map(_.layerNameString())
    (extensions.toSeq, layers.toSeq)

  lazy val version: Int = VK.getInstanceVersionSupported

private[cyfra] class Instance(enableValidationLayers: Boolean, enableSurfaceExtensions: Boolean = false) extends VulkanObject:

  private val instance: VkInstance = pushStack: stack =>
    try
      val appInfo = VkApplicationInfo
        .calloc(stack)
        .sType$Default()
        .pNext(NULL)
        .pApplicationName(stack.UTF8("cyfra MVP"))
        .pEngineName(stack.UTF8("cyfra Computing Engine"))
        .applicationVersion(VK_MAKE_VERSION(0, 1, 0))
        .engineVersion(VK_MAKE_VERSION(0, 1, 0))
        .apiVersion(Instance.version)

      val ppEnabledExtensionNames = getInstanceExtensions(stack, enableSurfaceExtensions)
      val ppEnabledLayerNames =
        val layers = enabledLayers
        val pointer = stack.callocPointer(layers.length)
        layers.foreach(x => pointer.put(stack.ASCII(x)))
        pointer.flip()

      val pCreateInfo = VkInstanceCreateInfo
        .calloc(stack)
        .sType$Default()
        .flags(VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR)
        .pNext(NULL)
        .pApplicationInfo(appInfo)
        .ppEnabledExtensionNames(ppEnabledExtensionNames)
        .ppEnabledLayerNames(ppEnabledLayerNames)
      val pInstance = stack.mallocPointer(1)
      val result = vkCreateInstance(pCreateInfo, null, pInstance)

      if result != VK_SUCCESS then throw new RuntimeException(s"Failed to create VkInstance: $result")

      val instanceHandle = pInstance.get(0)
      if instanceHandle == 0L then throw new RuntimeException("Created VkInstance handle is null")

      new VkInstance(instanceHandle, pCreateInfo)
    catch
      case e: Exception =>
        logger.error(s"Failed to create Vulkan instance: ${e.getMessage}")
        throw e

  lazy val enabledLayers: Seq[String] = List
    .empty[String]
    .pipe: x =>
      if Instance.layers.contains(ValidationLayer) && enableValidationLayers then ValidationLayer +: x
      else if enableValidationLayers then
        logger.error("Validation layers requested but not available")
        x
      else x

  def get: VkInstance = instance

  override protected def close(): Unit =
    vkDestroyInstance(instance, null)

  private def getInstanceExtensions(stack: MemoryStack, includeSurfaceExtensions: Boolean) =
    val n = stack.callocInt(1)
    check(vkEnumerateInstanceExtensionProperties(null.asInstanceOf[ByteBuffer], n, null))
    val buffer = VkExtensionProperties.calloc(n.get(0), stack)
    check(vkEnumerateInstanceExtensionProperties(null.asInstanceOf[ByteBuffer], n, buffer))

    val availableExtensions =
      val buf = mutable.Buffer[String]()
      buffer.forEach: ext =>
        buf.addOne(ext.extensionNameString())

      buf.toSet

    val extensions = mutable.Buffer.from(Instance.MoltenVkExtensions)
    if enableValidationLayers then extensions.addAll(Instance.ValidationLayersExtensions)

    if includeSurfaceExtensions then
      val glfwExtensions = GLFWVulkan.glfwGetRequiredInstanceExtensions()
      if glfwExtensions != null then
        val extensionNames = (0 until glfwExtensions.capacity()).map: i =>
          val extName = org.lwjgl.system.MemoryUtil.memUTF8(glfwExtensions.get(i))
          extensions.addOne(extName)
          extName
      else {}
    else {}

    val filteredExtensions = extensions.filter: ext =>
      availableExtensions
        .contains(ext)
        .tap: x =>
          if !x then logger.warn(s"Requested Vulkan instance extension '$ext' is not available")

    val ppEnabledExtensionNames = stack.callocPointer(filteredExtensions.size)
    filteredExtensions.foreach(x => ppEnabledExtensionNames.put(stack.ASCII(x)))
    ppEnabledExtensionNames.flip()
