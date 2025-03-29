package io.computenode.cyfra.api

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.{Shader => VkShader, LayoutInfo => VkLayoutInfo}
import org.joml.Vector3i
import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}
import io.computenode.cyfra.vulkan.compute.{LayoutSet => VkLayoutSet, Binding => VkBinding, InputBufferSize}
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Try, Success, Failure}
import java.io.{FileInputStream, IOException}
import java.nio.channels.FileChannel
import scala.util.Using

/**
 * Represents a compute shader
 */
class Shader(
    private[api] val vulkanContext: VulkanContext,
    spirvCode: ByteBuffer,
    workgroupSize: Vector3i,
    layoutInfo: LayoutInfo,
    entryPoint: String = "main"
) extends AutoCloseable {
  
  private val closed = new AtomicBoolean(false)
  
  // Convert to Vulkan layout info
  private val vkLayoutInfo = new VkLayoutInfo(
    layoutInfo.sets.map(set => 
      VkLayoutSet(set.id, set.bindings.map(binding => 
        VkBinding(binding.id, InputBufferSize(binding.size))
      ))
    )
  )
  
  private[api] val vkShader = new VkShader(
    spirvCode, 
    workgroupSize, 
    vkLayoutInfo, 
    entryPoint, 
    vulkanContext.device
  )
  
  /**
   * Get the working group dimensions
   * @return Vector3i containing the dimensions
   */
  def getWorkgroupDimensions: Vector3i = workgroupSize
  
  /**
   * Get the layout information
   * @return Layout information for this shader
   */
  def getLayoutInfo: LayoutInfo = layoutInfo
  
  /**
   * Get the entry point for this shader
   * @return The shader entry point function name
   */
  def getEntryPoint: String = entryPoint
  
  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      vkShader.close()
    }
  }
}

object Shader {
  /**
   * Load a shader from a file
   * @param context Vulkan context
   * @param path Path to the SPIR-V file
   * @param workgroupSize Size of the workgroup
   * @param layoutInfo Layout information
   * @param entryPoint Entry point function name
   * @return A new Shader instance
   */
  def loadFromFile(
    context: VulkanContext,
    path: String,
    workgroupSize: Vector3i,
    layoutInfo: LayoutInfo,
    entryPoint: String = "main"
  ): Try[Shader] = Try {
    val bytes = Files.readAllBytes(Paths.get(path))
    val buffer = ByteBuffer.allocateDirect(bytes.length)
    buffer.put(bytes)
    buffer.flip()
    
    new Shader(context, buffer, workgroupSize, layoutInfo, entryPoint)
  }
  
  /**
   * Load a shader from a resource
   * @param context Vulkan context
   * @param resourcePath Resource path to the SPIR-V file
   * @param workgroupSize Size of the workgroup
   * @param layoutInfo Layout information
   * @param entryPoint Entry point function name
   * @return A new Shader instance
   */
  def loadFromResource(
    context: VulkanContext,
    resourcePath: String,
    workgroupSize: Vector3i,
    layoutInfo: LayoutInfo,
    entryPoint: String = "main"
  ): Try[Shader] = Try {
    Using.resource(getClass.getClassLoader.getResourceAsStream(resourcePath)) { inputStream =>
      val channel = java.nio.channels.Channels.newChannel(inputStream)
      val buffer = ByteBuffer.allocateDirect(inputStream.available())
      channel.read(buffer)
      buffer.flip()
      new Shader(context, buffer, workgroupSize, layoutInfo, entryPoint)
    }
  }
}