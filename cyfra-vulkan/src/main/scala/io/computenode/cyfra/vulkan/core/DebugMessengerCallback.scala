package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.EXTDebugUtils.{
  VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
  VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
  VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT,
  VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
  VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT,
  VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
  VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
  vkCreateDebugUtilsMessengerEXT,
  vkDestroyDebugUtilsMessengerEXT,
}
import org.lwjgl.vulkan.VK10.VK_FALSE
import org.lwjgl.vulkan.{VkDebugUtilsMessengerCallbackDataEXT, VkDebugUtilsMessengerCallbackEXT, VkDebugUtilsMessengerCreateInfoEXT}
import org.slf4j.LoggerFactory

import java.lang.Integer.highestOneBit
import java.nio.LongBuffer

class DebugMessengerCallback(instance: Instance) extends VulkanObjectHandle:
  private val logger = LoggerFactory.getLogger("Cyfra-DebugMessenger")

  protected val handle: Long = pushStack: stack =>
    val callback =
      new VkDebugUtilsMessengerCallbackEXT():
        override def invoke(messageSeverity: Int, messageTypes: Int, pCallbackData: Long, pUserData: Long): Int =
          val message = VkDebugUtilsMessengerCallbackDataEXT.create(pCallbackData).pMessageString()
          val debugMessage = message.split("\\|").last
          highestOneBit(messageSeverity) match
            case VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT   => logger.error(debugMessage)
            case VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT => logger.warn(debugMessage)
            case VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT    => logger.info(debugMessage)
            case VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT => logger.debug(debugMessage)
            case x => logger.error(s"Unexpected message severity: $messageSeverity, message: $debugMessage")
          VK_FALSE

    val debugMessengerCreate = VkDebugUtilsMessengerCreateInfoEXT
      .calloc(stack)
      .sType$Default()
      .messageSeverity(
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT |
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      )
      .messageType(
        VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      )
      .pfnUserCallback(callback)

    val debugMessengerBuff = stack.callocLong(1)
    check(vkCreateDebugUtilsMessengerEXT(instance.get, debugMessengerCreate, null, debugMessengerBuff), "Failed to create debug messenger")
    debugMessengerBuff.get()

  override protected def close(): Unit = vkDestroyDebugUtilsMessengerEXT(instance.get, handle, null)
