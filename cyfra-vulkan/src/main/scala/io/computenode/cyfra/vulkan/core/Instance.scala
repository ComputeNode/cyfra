package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.core.Instance.ValidationLayer
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.system.{MemoryStack, MemoryUtil}
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.EXTDebugReport.VK_EXT_DEBUG_REPORT_EXTENSION_NAME
import org.lwjgl.vulkan.EXTLayerSettings.{VK_LAYER_SETTING_TYPE_BOOL32_EXT, VK_LAYER_SETTING_TYPE_STRING_EXT, VK_LAYER_SETTING_TYPE_UINT32_EXT}
import org.lwjgl.vulkan.KHRPortabilityEnumeration.{VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR, VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME}
import org.lwjgl.vulkan.EXTLayerSettings.VK_EXT_LAYER_SETTINGS_EXTENSION_NAME
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.EXTValidationFeatures.*
import org.lwjgl.vulkan.EXTDebugUtils.*

import java.nio.{ByteBuffer, LongBuffer}
import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.util.chaining.*

/** @author
  *   MarconZet Created 13.04.2020
  */
object Instance:
  private val ValidationLayer: String = "VK_LAYER_KHRONOS_validation"
  private val ValidationLayersExtensions: Seq[String] =
    List(VK_EXT_DEBUG_REPORT_EXTENSION_NAME, VK_EXT_DEBUG_UTILS_EXTENSION_NAME, VK_EXT_LAYER_SETTINGS_EXTENSION_NAME)
  private val MoltenVkExtensions: Seq[String] = List(VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME)

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

private[cyfra] class Instance(enableValidationLayers: Boolean, enablePrinting: Boolean) extends VulkanObject[VkInstance]:

  protected val handle: VkInstance = pushStack: stack =>
    val appInfo = VkApplicationInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .pApplicationName(stack.UTF8("cyfra MVP"))
      .pEngineName(stack.UTF8("cyfra Computing Engine"))
      .applicationVersion(VK_MAKE_VERSION(0, 1, 0))
      .engineVersion(VK_MAKE_VERSION(0, 1, 0))
      .apiVersion(Instance.version)

    val ppEnabledExtensionNames = getInstanceExtensions(stack)
    val ppEnabledLayerNames =
      val pointer = stack.callocPointer(enabledLayers.length)
      enabledLayers.foreach(x => pointer.put(stack.ASCII(x)))
      pointer.flip()

    val pCreateInfo = VkInstanceCreateInfo
      .calloc(stack)
      .sType$Default()
      .flags(VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR)
      .pNext(0)
      .pApplicationInfo(appInfo)
      .ppEnabledExtensionNames(ppEnabledExtensionNames)
      .ppEnabledLayerNames(ppEnabledLayerNames)

    if enableValidationLayers then
      val layerSettings = VkLayerSettingEXT.calloc(10, stack)

      setTrue(layerSettings.get(), "validate_sync", stack)
      setTrue(layerSettings.get(), "gpuav_enable", stack)
      setTrue(layerSettings.get(), "validate_best_practices", stack)

      if enablePrinting then
        setTrue(layerSettings.get(), "printf_enable", stack)

        layerSettings
          .get()
          .pLayerName(stack.ASCII(ValidationLayer))
          .pSettingName(stack.ASCII("printf_buffer_size"))
          .`type`(VK_LAYER_SETTING_TYPE_UINT32_EXT)
          .valueCount(1)
          .pValues(MemoryUtil.memByteBuffer(stack.ints(1024 * 1024)))

      layerSettings.flip()

      val layerSettingsCI = VkLayerSettingsCreateInfoEXT.calloc(stack).sType$Default().pSettings(layerSettings)

      pCreateInfo.pNext(layerSettingsCI)

    val pInstance = stack.mallocPointer(1)
    check(vkCreateInstance(pCreateInfo, null, pInstance), "Failed to create VkInstance")
    new VkInstance(pInstance.get(0), pCreateInfo)

  lazy val enabledLayers: Seq[String] = List
    .empty[String]
    .pipe: x =>
      if Instance.layers.contains(ValidationLayer) && enableValidationLayers then ValidationLayer +: x
      else if enableValidationLayers then
        logger.error("Validation layers requested but not available")
        x
      else x

  override protected def close(): Unit =
    vkDestroyInstance(handle, null)

  private def getInstanceExtensions(stack: MemoryStack) =
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

    val filteredExtensions = extensions.filter(ext =>
      availableExtensions
        .contains(ext)
        .tap: x => // TODO better handle missing extensions
          if !x then logger.warn(s"Requested Vulkan instance extension '$ext' is not available"),
    )

    val ppEnabledExtensionNames = stack.callocPointer(extensions.size)
    filteredExtensions.foreach(x => ppEnabledExtensionNames.put(stack.ASCII(x)))
    ppEnabledExtensionNames.flip()

  private def setTrue(setting: VkLayerSettingEXT, name: String, stack: MemoryStack) =
    setting
      .pLayerName(stack.ASCII(ValidationLayer))
      .pSettingName(stack.ASCII(name))
      .`type`(VK_LAYER_SETTING_TYPE_BOOL32_EXT)
      .valueCount(1)
      .pValues(MemoryUtil.memByteBuffer(stack.ints(1)))
