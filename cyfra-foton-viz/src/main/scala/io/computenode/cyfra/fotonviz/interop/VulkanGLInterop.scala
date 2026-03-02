package io.computenode.cyfra.fotonviz.interop

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL30.{glGetStringi, GL_NUM_EXTENSIONS}
import org.lwjgl.opengl.GL45.*
import org.lwjgl.opengl.EXTMemoryObject.*
import org.lwjgl.opengl.EXTMemoryObjectWin32.*
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK11.*
import org.lwjgl.vulkan.KHRExternalMemory.*
import org.lwjgl.vulkan.KHRExternalMemoryWin32.*

import java.nio.LongBuffer

/** Vulkan-OpenGL interop for zero-copy buffer sharing. */
object VulkanGLInterop:

  /** Check if OpenGL supports the required extensions */
  def checkGLSupport(): Boolean =
    val numExtensions = glGetInteger(GL_NUM_EXTENSIONS)
    val extensions = (0 until numExtensions).map(i => glGetStringi(GL_EXTENSIONS, i)).toSet
    val required = Set(
      "GL_EXT_memory_object",
      "GL_EXT_memory_object_win32",
    )
    val missing = required -- extensions
    if missing.nonEmpty then
      println(s"Missing OpenGL extensions: $missing")
      false
    else
      true

/** A buffer shared between Vulkan and OpenGL via external memory. */
class SharedBuffer(val size: Int, vkDevice: VkDevice, vkPhysicalDevice: VkPhysicalDevice) extends AutoCloseable:

  private val (vkBuffer, vkMemory, memoryHandle) = createVulkanBuffer()
  private val (glMemoryObject, glBuffer) = createGLBuffer()

  def vulkanBuffer: Long = vkBuffer
  def glBufferHandle: Int = glBuffer

  private def createVulkanBuffer(): (Long, Long, Long) = pushStack: stack =>
    val externalMemoryInfo = VkExternalMemoryBufferCreateInfo.calloc(stack)
      .sType$Default()
      .handleTypes(VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT)

    val bufferInfo = VkBufferCreateInfo.calloc(stack)
      .sType$Default()
      .pNext(externalMemoryInfo)
      .size(size.toLong)
      .usage(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT)
      .sharingMode(VK_SHARING_MODE_EXCLUSIVE)

    val pBuffer = stack.mallocLong(1)
    check(vkCreateBuffer(vkDevice, bufferInfo, null, pBuffer), "Failed to create shared buffer")
    val buffer = pBuffer.get(0)

    val memReqs = VkMemoryRequirements.calloc(stack)
    vkGetBufferMemoryRequirements(vkDevice, buffer, memReqs)

    val memProps = VkPhysicalDeviceMemoryProperties.calloc(stack)
    vkGetPhysicalDeviceMemoryProperties(vkPhysicalDevice, memProps)

    val memoryTypeIndex = findMemoryType(
      memReqs.memoryTypeBits(),
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
      memProps,
    )

    val exportInfo = VkExportMemoryAllocateInfo.calloc(stack)
      .sType$Default()
      .handleTypes(VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT)

    val allocInfo = VkMemoryAllocateInfo.calloc(stack)
      .sType$Default()
      .pNext(exportInfo)
      .allocationSize(memReqs.size())
      .memoryTypeIndex(memoryTypeIndex)

    val pMemory = stack.mallocLong(1)
    check(vkAllocateMemory(vkDevice, allocInfo, null, pMemory), "Failed to allocate shared memory")
    val memory = pMemory.get(0)

    check(vkBindBufferMemory(vkDevice, buffer, memory, 0), "Failed to bind buffer memory")

    val handleInfo = VkMemoryGetWin32HandleInfoKHR.calloc(stack)
      .sType$Default()
      .memory(memory)
      .handleType(VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT)

    val pHandle = stack.mallocPointer(1)
    check(
      KHRExternalMemoryWin32.vkGetMemoryWin32HandleKHR(vkDevice, handleInfo, pHandle),
      "Failed to get Win32 memory handle",
    )

    (buffer, memory, pHandle.get(0))

  private def createGLBuffer(): (Int, Int) =
    val memObj = glCreateMemoryObjectsEXT()
    glImportMemoryWin32HandleEXT(memObj, size.toLong, GL_HANDLE_TYPE_OPAQUE_WIN32_EXT, memoryHandle)

    val error = glGetError()
    if error != GL_NO_ERROR then
      throw new RuntimeException(s"OpenGL error importing memory: $error")

    val buffer = glCreateBuffers()
    glNamedBufferStorageMemEXT(buffer, size.toLong, memObj, 0)

    val error2 = glGetError()
    if error2 != GL_NO_ERROR then
      throw new RuntimeException(s"OpenGL error creating buffer: $error2")

    (memObj, buffer)

  private def findMemoryType(typeFilter: Int, properties: Int, memProps: VkPhysicalDeviceMemoryProperties): Int =
    val result = (0 until memProps.memoryTypeCount()).find: i =>
      (typeFilter & (1 << i)) != 0 &&
        (memProps.memoryTypes(i).propertyFlags() & properties) == properties
    result.getOrElse(throw new RuntimeException("Failed to find suitable memory type"))

  /** Copy from a source Vulkan buffer to this shared buffer (GPU-GPU copy). */
  def copyFrom(srcBuffer: Long, queue: VkQueue, commandPool: Long): Unit = pushStack: stack =>
    val allocInfo = VkCommandBufferAllocateInfo.calloc(stack)
      .sType$Default()
      .commandPool(commandPool)
      .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
      .commandBufferCount(1)
    
    val pCommandBuffer = stack.mallocPointer(1)
    check(vkAllocateCommandBuffers(vkDevice, allocInfo, pCommandBuffer), "Failed to allocate command buffer")
    val commandBuffer = new VkCommandBuffer(pCommandBuffer.get(0), vkDevice)
    
    val beginInfo = VkCommandBufferBeginInfo.calloc(stack)
      .sType$Default()
      .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
    check(vkBeginCommandBuffer(commandBuffer, beginInfo), "Failed to begin command buffer")
    
    val copyRegion = VkBufferCopy.calloc(1, stack)
      .srcOffset(0)
      .dstOffset(0)
      .size(size.toLong)
    vkCmdCopyBuffer(commandBuffer, srcBuffer, vkBuffer, copyRegion)
    
    check(vkEndCommandBuffer(commandBuffer), "Failed to end command buffer")
    
    val pCB = stack.pointers(commandBuffer)
    val submitInfo = VkSubmitInfo.calloc(stack)
      .sType$Default()
      .pCommandBuffers(pCB)
    
    check(vkQueueSubmit(queue, submitInfo, VK_NULL_HANDLE), "Failed to submit copy")
    vkQueueWaitIdle(queue)
    
    vkFreeCommandBuffers(vkDevice, commandPool, pCB)

  override def close(): Unit =
    glDeleteBuffers(glBuffer)
    glDeleteMemoryObjectsEXT(glMemoryObject)
    vkDestroyBuffer(vkDevice, vkBuffer, null)
    vkFreeMemory(vkDevice, vkMemory, null)
