package io.computenode.cyfra.vulkan.core

import DebugCallback.DEBUG_REPORT
import io.computenode.cyfra.vulkan.util.Util.check
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.vulkan.EXTDebugReport.*
import org.lwjgl.vulkan.VK10.VK_SUCCESS
import org.lwjgl.vulkan.{VkDebugReportCallbackCreateInfoEXT, VkDebugReportCallbackEXT}
import org.slf4j.{Logger, LoggerFactory}

import java.lang.Integer.highestOneBit
import java.nio.LongBuffer

/** @author
  *   MarconZet Created 13.04.2020
  */
object DebugCallback {
  val DEBUG_REPORT = VK_DEBUG_REPORT_ERROR_BIT_EXT | VK_DEBUG_REPORT_WARNING_BIT_EXT | VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
}

private[cyfra] class DebugCallback(instance: Instance) extends VulkanObjectHandle {
  override protected val handle: Long = {
    val debugCallback = new VkDebugReportCallbackEXT() {
      def invoke(flags: Int, objectType: Int, `object`: Long, location: Long, messageCode: Int, pLayerPrefix: Long, pMessage: Long, pUserData: Long)
        : Int = {
        val decodedMessage = VkDebugReportCallbackEXT.getString(pMessage)
        highestOneBit(flags) match {
          case VK_DEBUG_REPORT_DEBUG_BIT_EXT =>
            logger.debug(decodedMessage)
          case VK_DEBUG_REPORT_ERROR_BIT_EXT =>
            logger.error(decodedMessage, new RuntimeException())
          case VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT | VK_DEBUG_REPORT_WARNING_BIT_EXT =>
            logger.warn(decodedMessage)
          case VK_DEBUG_REPORT_INFORMATION_BIT_EXT =>
            logger.info(decodedMessage)
          case x => logger.error(s"Unexpected value: x, message: $decodedMessage")
        }
        0
      }
    }
    setupDebugging(DEBUG_REPORT, debugCallback)
  }
  private val logger = LoggerFactory.getLogger(this.getClass)

  override protected def close(): Unit =
    vkDestroyDebugReportCallbackEXT(instance.get, handle, null)

  private def setupDebugging(flags: Int, callback: VkDebugReportCallbackEXT): Long = {
    val dbgCreateInfo = VkDebugReportCallbackCreateInfoEXT
      .create()
      .sType$Default()
      .pNext(NULL)
      .pfnCallback(callback)
      .pUserData(NULL)
      .flags(flags)
    val pCallback = BufferUtils.createLongBuffer(1)
    val err = vkCreateDebugReportCallbackEXT(instance.get, dbgCreateInfo, null, pCallback)
    val callbackHandle = pCallback.get(0)
    if (err != VK_SUCCESS)
      throw new VulkanAssertionError("Failed to create DebugCallback", err)
    callbackHandle
  }
}
