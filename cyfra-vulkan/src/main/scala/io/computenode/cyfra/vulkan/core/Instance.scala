package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext.{SyncLayer, ValidationLayer}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.EXTDebugReport.VK_EXT_DEBUG_REPORT_EXTENSION_NAME
import org.lwjgl.vulkan.KHRPortabilityEnumeration.{VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR, VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME}
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK13.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.util.chaining.*

/** @author
  *   MarconZet Created 13.04.2020
  */
object Instance {
  val ValidationLayersExtensions: Seq[String] = List(VK_EXT_DEBUG_REPORT_EXTENSION_NAME)
  val MacOsExtensions: Seq[String] = List(VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME)

  lazy val (extensions, layers): (Seq[String], Seq[String]) = pushStack { stack =>
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
  }

  lazy val version: Int = VK.getInstanceVersionSupported
}

private[cyfra] class Instance(enableValidationLayers: Boolean) extends VulkanObject {

  private val instance: VkInstance = pushStack { stack =>

    val appInfo = VkApplicationInfo
      .calloc(stack)
      .sType$Default()
      .pNext(NULL)
      .pApplicationName(stack.UTF8("cyfra MVP"))
      .pEngineName(stack.UTF8("cyfra Computing Engine"))
      .applicationVersion(VK_MAKE_VERSION(0, 1, 0))
      .engineVersion(VK_MAKE_VERSION(0, 1, 0))
      .apiVersion(Instance.version)

    val ppEnabledExtensionNames = getInstanceExtensions(stack)
    val ppEnabledLayerNames = {
      val layers = enabledLayers
      val pointer = stack.callocPointer(layers.length)
      layers.foreach(x => pointer.put(stack.ASCII(x)))
      pointer.flip()
    }

    val pCreateInfo = VkInstanceCreateInfo
      .calloc(stack)
      .sType$Default()
      .flags(VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR)
      .pNext(NULL)
      .pApplicationInfo(appInfo)
      .ppEnabledExtensionNames(ppEnabledExtensionNames)
      .ppEnabledLayerNames(ppEnabledLayerNames)
    val pInstance = stack.mallocPointer(1)
    check(vkCreateInstance(pCreateInfo, null, pInstance), "Failed to create VkInstance")
    new VkInstance(pInstance.get(0), pCreateInfo)
  }

  lazy val enabledLayers: Seq[String] = List
    .empty[String]
    .pipe { x =>
      if (Instance.layers.contains(ValidationLayer) && enableValidationLayers) ValidationLayer +: x
      else if (enableValidationLayers)
        println("Validation layers requested but not available")
        x
      else x
    }

  def get: VkInstance = instance

  override protected def close(): Unit =
    vkDestroyInstance(instance, null)

  private def getInstanceExtensions(stack: MemoryStack) = {
    val extensions = mutable.Buffer.from(Instance.MacOsExtensions)
    if (enableValidationLayers)
      extensions.addAll(Instance.ValidationLayersExtensions)

    val ppEnabledExtensionNames = stack.callocPointer(extensions.size)
    extensions.foreach(x => ppEnabledExtensionNames.put(stack.ASCII(x)))
    ppEnabledExtensionNames.flip()
  }
}
