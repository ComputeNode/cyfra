package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.utility.Logger.logger
import io.computenode.cyfra.vulkan.core.DebugReportCallback.DebugReport
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.vulkan.EXTDebugReport.*
import org.lwjgl.vulkan.VK10.VK_SUCCESS
import org.lwjgl.vulkan.{VkDebugReportCallbackCreateInfoEXT, VkDebugReportCallbackEXT}
import org.slf4j.LoggerFactory

import java.lang.Integer.highestOneBit

/** @author
  *   MarconZet Created 13.04.2020
  */
object DebugReportCallback:
  val DebugReport: Int = VK_DEBUG_REPORT_ERROR_BIT_EXT | VK_DEBUG_REPORT_WARNING_BIT_EXT | VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT

private[cyfra] class DebugReportCallback(instance: Instance) extends VulkanObjectHandle:
  private val logger = LoggerFactory.getLogger("Cyfra-DebugReport")

  protected val handle: Long = pushStack: stack =>
    val debugCallback = new VkDebugReportCallbackEXT():
      def invoke(
        flags: Int,
        objectType: Int,
        `object`: Long,
        location: Long,
        messageCode: Int,
        pLayerPrefix: Long,
        pMessage: Long,
        pUserData: Long,
      ): Int =
        val decodedMessage = VkDebugReportCallbackEXT.getString(pMessage)
        highestOneBit(flags) match
          case VK_DEBUG_REPORT_DEBUG_BIT_EXT                                                 => logger.debug(decodedMessage)
          case VK_DEBUG_REPORT_ERROR_BIT_EXT                                                 => logger.error(decodedMessage)
          case VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT | VK_DEBUG_REPORT_WARNING_BIT_EXT => logger.warn(decodedMessage)
          case VK_DEBUG_REPORT_INFORMATION_BIT_EXT                                           => logger.info(decodedMessage)
          case x => logger.error(s"Unexpected value: x, message: $decodedMessage")
        0

    val dbgCreateInfo = VkDebugReportCallbackCreateInfoEXT
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .pfnCallback(debugCallback)
      .pUserData(0)
      .flags(DebugReport)
    val pCallback = stack.callocLong(1)
    check(vkCreateDebugReportCallbackEXT(instance.get, dbgCreateInfo, null, pCallback), "Failed to create DebugCallback")
    pCallback.get()

  override protected def close(): Unit =
    vkDestroyDebugReportCallbackEXT(instance.get, handle, null)
