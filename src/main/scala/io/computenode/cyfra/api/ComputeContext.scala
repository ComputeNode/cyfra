package io.computenode.cyfra.api

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.memory.Buffer as VkBuffer
import org.joml.Vector3i
import java.nio.ByteBuffer
import io.computenode.cyfra.vulkan.executor.SequenceExecutor
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.{Compute, LayoutLocation, Dependency}
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal
import io.computenode.cyfra.vulkan.executor.BufferAction
import org.lwjgl.vulkan.VK10.* 
import org.lwjgl.util.vma.Vma.*

/**
 * High-level abstraction for GPU computation operations.
 * Manages Vulkan resources and provides a simplified API for compute operations.
 */
class ComputeContext(enableValidation: Boolean = false) extends AutoCloseable {
  private val vulkanContext = new VulkanContext(enableValidation)
  private val executionLock = new ReentrantLock()
  private val closed = new AtomicBoolean(false)
  
  /**
   * Creates a buffer in GPU memory
   * @param size Size of the buffer in bytes
   * @param isHostVisible Whether the buffer should be host-visible for direct mapping
   * @return A new Buffer instance
   * @throws IllegalStateException if context has been closed
   */
  def createBuffer(size: Int, isHostVisible: Boolean = false): Try[Buffer] = Try {
    val usageFlags = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | 
                    VK_BUFFER_USAGE_TRANSFER_SRC_BIT | 
                    VK_BUFFER_USAGE_TRANSFER_DST_BIT
                    
    val memoryUsage = if (isHostVisible) {
      VMA_MEMORY_USAGE_CPU_TO_GPU
    } else {
      VMA_MEMORY_USAGE_GPU_ONLY
    }
    
    val vkBuffer = new io.computenode.cyfra.vulkan.memory.Buffer(
      size,
      usageFlags,
      if (isHostVisible) VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT else 0,
      memoryUsage,
      vulkanContext.allocator
    )
    
    new Buffer(vulkanContext, size, isHostVisible)
  }
  
  /**
   * Creates a shader from SPIR-V binary
   * @param spirvCode SPIR-V binary code
   * @param workgroupSize Size of the workgroup for this shader
   * @param layout Layout information for descriptor sets
   * @param entryPoint Entry point function name
   * @return A new Shader instance
   * @throws IllegalStateException if context has been closed
   */
  def createShader(
    spirvCode: ByteBuffer, 
    workgroupSize: Vector3i,
    layout: LayoutInfo, 
    entryPoint: String = "main"
  ): Try[Shader] = Try {
    if (closed.get()) {
      throw new IllegalStateException("ComputeContext has been closed")
    }
    
    new Shader(vulkanContext, spirvCode, workgroupSize, layout, entryPoint)
  }
  
  /**
   * Creates a pipeline from a shader
   * @param shader The shader to use in this pipeline
   * @return A new Pipeline instance
   * @throws IllegalStateException if context has been closed
   */
  def createPipeline(shader: Shader): Try[Pipeline] = Try {
    if (closed.get()) {
      throw new IllegalStateException("ComputeContext has been closed")
    }
    
    new Pipeline(vulkanContext, shader)
  }
  
  /**
   * Executes a pipeline with the given input/output buffers
   * @param pipeline The pipeline to execute
   * @param inputs Input buffers
   * @param outputs Output buffers
   * @param elemCount Number of elements to process
   * @throws IllegalStateException if context has been closed
   */
  def execute(
    pipeline: Pipeline, 
    inputs: Seq[Buffer], 
    outputs: Seq[Buffer], 
    elemCount: Int
  ): Try[Unit] = Try {
    if (closed.get()) {
      throw new IllegalStateException("ComputeContext has been closed")
    }
    
    executionLock.lock()
    try {
      val bufferActions = Map(
        LayoutLocation(0, 0) -> BufferAction.LoadTo,
        LayoutLocation(1, 0) -> BufferAction.LoadFrom
      )

      val computeOp = Compute(pipeline.vkPipeline, bufferActions)
      val sequence = SequenceExecutor.ComputationSequence(Seq(computeOp), Seq())
      
      val executor = try {
        new SequenceExecutor(sequence, vulkanContext)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          throw e
      }

      try {
        // Map buffers with proper position reset
        val inBuffers = inputs.map { buffer =>
          val byteBuffer = buffer.vkBuffer.mapToByteBuffer()
          // Important: Reset position to ensure the entire buffer is available
          byteBuffer.position(0)
          byteBuffer
        }
        
        // Before execution, validate buffers
        inBuffers.zipWithIndex.foreach { case (buf, idx) =>
          if (buf == null || !buf.isDirect || buf.capacity() < 4 * elemCount) {
            throw new IllegalArgumentException(s"Invalid input buffer $idx: ${if (buf == null) "null" else s"direct=${buf.isDirect}, capacity=${buf.capacity()}"}")
          }
        }
        
        // Execute
        val results = executor.execute(inBuffers, elemCount)
        
        // Process results
        outputs.zipWithIndex.foreach { case (buffer, idx) =>
          if (idx < results.length) {
            buffer.copyFrom(results(idx))
          } else {
            throw new RuntimeException(s"Missing result for output buffer $idx")
          }
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
          throw new RuntimeException("Failed to execute pipeline", e)
      } finally {
        executor.destroy()
      }
    } finally {
      executionLock.unlock()
    }
  }
  
  /**
   * Create and execute a pipeline sequence with dependencies
   * @param operations List of (pipeline, inputs, outputs) operations
   * @param dependencies List of (fromPipeline, fromSet, toPipeline, toSet) dependencies
   * @param elemCount Number of elements to process
   * @throws IllegalStateException if context has been closed
   */
  def executeSequence(
    operations: Seq[(Pipeline, Seq[Buffer], Seq[Buffer])],
    dependencies: Seq[(Pipeline, Int, Pipeline, Int)],
    elemCount: Int
  ): Try[Seq[Buffer]] = Try {
    if (closed.get()) {
      throw new IllegalStateException("ComputeContext has been closed")
    }
    
    executionLock.lock()
    try {
      // Map operations to Compute instances with buffer actions
      val computeOps = operations.map { case (pipeline, inputs, outputs) =>
        val actions = (inputs.zipWithIndex.map { case (_, idx) => 
            LayoutLocation(0, idx) -> BufferAction.LoadTo 
          } ++ 
          outputs.zipWithIndex.map { case (_, idx) => 
            LayoutLocation(1, idx) -> BufferAction.LoadFrom 
          }).toMap
        
        Compute(pipeline.vkPipeline, actions)
      }
      
      // Map dependencies
      val vkDependencies = dependencies.map { case (fromPipeline, fromSet, toPipeline, toSet) =>
        Dependency(fromPipeline.vkPipeline, fromSet, toPipeline.vkPipeline, toSet)
      }
      
      val sequence = SequenceExecutor.ComputationSequence(computeOps, vkDependencies)
      val executor = new SequenceExecutor(sequence, vulkanContext)
      
      try {
        // Use mapToByteBuffer instead of getData
        val inBuffers = operations.flatMap(_._2).map { buf =>
          val mappedBuf = buf.vkBuffer.mapToByteBuffer()
          mappedBuf
        }.distinct
        
        val results = executor.execute(inBuffers, elemCount)
        
        // Create new output buffers with results
        results.map { byteBuffer =>
          createBuffer(byteBuffer.remaining()) match {
            case Success(buffer) =>
              buffer.copyFrom(byteBuffer) match {
                case Success(_) => buffer
                case Failure(e) => throw new RuntimeException("Failed to copy result to output buffer", e)
              }
            case Failure(e) => throw new RuntimeException("Failed to create output buffer", e)
          }
        }
      } finally {
        executor.destroy()
      }
    } finally {
      executionLock.unlock()
    }
  }
  
  /**
   * Create buffer with the specified data
   * @param data The data to copy to the buffer
   * @param isHostVisible Whether the buffer should be host-visible
   * @return A new Buffer instance initialized with the provided data
   */
  def createBufferWithData(data: ByteBuffer, isHostVisible: Boolean = false): Try[Buffer] = 
    for {
      buffer <- createBuffer(data.remaining(), isHostVisible)
      _ <- buffer.copyFrom(data)
    } yield buffer
  
  /**
   * Create an int buffer with the given array of integers
   * @param data The integer data
   * @param isHostVisible Whether the buffer should be host-visible
   * @return A buffer containing the integer data
   */
  def createIntBuffer(data: Array[Int], isHostVisible: Boolean = false): Try[Buffer] = Try {
    val byteBuffer = ByteBuffer.allocateDirect(data.length * 4)
    data.foreach(byteBuffer.putInt)
    byteBuffer.flip()
    
    createBufferWithData(byteBuffer, isHostVisible).get
  }
  
  /**
   * Create a float buffer with the given array of floats
   * @param data The float data
   * @param isHostVisible Whether the buffer should be host-visible
   * @return A buffer containing the float data
   */
  def createFloatBuffer(data: Array[Float], isHostVisible: Boolean = false): Try[Buffer] = Try {
    val byteBuffer = ByteBuffer.allocateDirect(data.length * 4)
    data.foreach(byteBuffer.putFloat)
    byteBuffer.flip()
    
    createBufferWithData(byteBuffer, isHostVisible).get
  }
  
  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      vulkanContext.destroy()
    }
  }
}