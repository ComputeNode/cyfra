package io.computenode.cyfra.rtrp.graphics

import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VkShaderModuleCreateInfo
import java.nio.ByteBuffer

private[cyfra] class Shader(
    shaderCode: ByteBuffer,
    val layoutInfo: LayoutInfo,
    val functionName: String,
    device: Device
) extends VulkanObjectHandle:
  
  protected val handle: Long = stackPush: stack =>
    val moduleCreateInfo = VkShaderModuleCreateInfo
      .calloc(stack)
      .sType$Default()
      .codeSize(shaderCode.capacity());
      .pCode(shaderCode)

    val pShaderModule = stack.mallocLong(1)
    if (vkCreateShaderModule(device.handle, moduleCreateInfo, null, pShaderModule) != VK_SUCCESS)
      throw new RuntimeException("Failed to create shader module")
    pShaderModule.get(0)

  
  override protected def close(): Unit = 
    vkDestroyShaderModule(device.handle, handle, null)

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
