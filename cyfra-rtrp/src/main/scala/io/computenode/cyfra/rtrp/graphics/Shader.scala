package io.computenode.cyfra.rtrp.graphics

import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.compute.LayoutInfo
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.joml.Vector3ic
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkShaderModuleCreateInfo

import java.io.{File, FileInputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.Objects


private[cyfra] class Shader(
    shaderCode: ByteBuffer,
    val functionName: String,
    device: Device
) extends VulkanObjectHandle:
  
  protected val handle: Long = pushStack: stack =>
    val moduleCreateInfo = VkShaderModuleCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .pCode(shaderCode)

    val pShaderModule = stack.mallocLong(1)
    if (vkCreateShaderModule(device.get, moduleCreateInfo, null, pShaderModule) != VK_SUCCESS)
      throw new RuntimeException("Failed to create shader module")
    pShaderModule.get(0)

  
  override protected def close(): Unit = 
    vkDestroyShaderModule(device.get, handle, null)

object Shader:

  def loadShader(path: String): ByteBuffer =
    loadShader(path, getClass.getClassLoader)

  private def loadShader(path: String, classLoader: ClassLoader): ByteBuffer =
    val stream = classLoader.getResourceAsStream(path)
    if stream == null then throw new RuntimeException(s"Shader resource not found: $path")
    val bytes = stream.readAllBytes()
    ByteBuffer.wrap(bytes)
