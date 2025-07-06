package io.computenode.cyfra.vulkan.archive.compute

import io.computenode.cyfra.vulkan.archive.core.Device
import io.computenode.cyfra.vulkan.archive.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.archive.util.VulkanObjectHandle
import org.joml.Vector3ic
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkShaderModuleCreateInfo

import java.io.{File, FileInputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.Objects

/** @author
  *   MarconZet Created 25.04.2020
  */
private[cyfra] class Shader(
  shaderCode: ByteBuffer,
  val workgroupDimensions: Vector3ic,
  val layoutInfo: LayoutInfo,
  val functionName: String,
  device: Device,
) extends VulkanObjectHandle:

  protected val handle: Long = pushStack: stack =>
    val shaderModuleCreateInfo = VkShaderModuleCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .pCode(shaderCode)

    val pShaderModule = stack.callocLong(1)
    check(vkCreateShaderModule(device.get, shaderModuleCreateInfo, null, pShaderModule), "Failed to create shader module")
    pShaderModule.get()

  protected def close(): Unit =
    vkDestroyShaderModule(device.get, handle, null)

object Shader:

  def loadShader(path: String): ByteBuffer =
    loadShader(path, getClass.getClassLoader)

  private def loadShader(path: String, classLoader: ClassLoader): ByteBuffer =
    try
      val file = new File(Objects.requireNonNull(classLoader.getResource(path)).getFile)
      val fis = new FileInputStream(file)
      val fc = fis.getChannel
      fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size())
    catch
      case e: IOException =>
        throw new RuntimeException(e)
