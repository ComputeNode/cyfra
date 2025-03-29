package io.computenode.cyfra.api

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.{ComputePipeline => VkComputePipeline}
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Try, Success, Failure}
import org.joml.Vector3i

/**
 * Represents a compute pipeline
 */
class Pipeline(
    private[api] val vulkanContext: VulkanContext,
    shader: Shader
) extends AutoCloseable {
  
  private val closed = new AtomicBoolean(false)
  
  // Create underlying Vulkan pipeline
  private[api] val vkPipeline = new VkComputePipeline(shader.vkShader, vulkanContext)
  
  /**
   * Get the shader used by this pipeline
   * @return The shader instance
   */
  def getShader: Shader = shader
  
  /**
   * Calculate the optimal workgroup count based on element count
   * @param elementCount Total number of elements to process
   * @return Optimal workgroup count
   */
  def calculateWorkgroupCount(elementCount: Int): Vector3i = {
    val workgroupSize = shader.getWorkgroupDimensions
    val x = Math.ceil(elementCount.toDouble / workgroupSize.x).toInt
    new Vector3i(x, 1, 1)
  }
  
  /**
   * Execute the pipeline with specified buffers
   * @param context The compute context
   * @param inputs Input buffers
   * @param outputs Output buffers
   * @param elementCount Number of elements to process
   * @throws IllegalStateException if pipeline has been closed
   */
  def execute(
    context: ComputeContext,
    inputs: Seq[Buffer],
    outputs: Seq[Buffer],
    elementCount: Int
  ): Try[Unit] = Try {
    if (closed.get()) {
      throw new IllegalStateException("Pipeline has been closed")
    }
    
    context.execute(this, inputs, outputs, elementCount)
  }
  
  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      vkPipeline.close()
    }
  }
}