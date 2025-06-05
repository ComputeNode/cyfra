package io.computenode.cyfra.vulkan.compute

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.joml.Vector3ic
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkShaderModuleCreateInfo

import java.io.{File, FileInputStream, IOException}
import java.nio.channels.FileChannel
import java.nio.{ByteBuffer, LongBuffer}
import java.util.stream.Collectors
import java.util.{List, Objects}
import scala.util.Using

/** @author
  *   MarconZet Created 25.04.2020
  */
private[cyfra] class Shader(shaderCode: ByteBuffer, val workgroupDimensions: Vector3ic, val layoutInfo: LayoutInfo, val functionName: String, device: Device)
    extends VulkanObjectHandle {

  protected val handle: Long = pushStack { stack =>
    val shaderModuleCreateInfo = VkShaderModuleCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .pCode(shaderCode)

    val pShaderModule = stack.callocLong(1)
    check(vkCreateShaderModule(device.get, shaderModuleCreateInfo, null, pShaderModule), "Failed to create shader module")
    pShaderModule.get()
  }

  protected def close(): Unit =
    vkDestroyShaderModule(device.get, handle, null)
}

object Shader {

  def loadShader(path: String): ByteBuffer =
    loadShader(path, getClass.getClassLoader)

  private def loadShader(path: String, classLoader: ClassLoader): ByteBuffer =
    try {
      val file = new File(Objects.requireNonNull(classLoader.getResource(path)).getFile)
      val fis = new FileInputStream(file)
      val fc = fis.getChannel
      fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size())
    } catch
      case e: IOException =>
        throw new RuntimeException(e)

}
