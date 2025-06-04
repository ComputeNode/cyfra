package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.compute.{Binding, ComputePipeline, InputBufferSize, LayoutInfo, LayoutSet, Shader}
import io.computenode.cyfra.vulkan.executor.BufferAction.{LoadFrom, LoadTo}
import io.computenode.cyfra.vulkan.executor.SequenceExecutor
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.{ComputationSequence, Compute, Dependency, LayoutLocation}
import io.computenode.cyfra.vulkan.memory.Buffer
import munit.FunSuite
import org.lwjgl.BufferUtils
import org.lwjgl.vulkan.VK10.* 
import org.lwjgl.util.vma.Vma.* 

class SequenceExecutorTest extends FunSuite:
  private val vulkanContext = VulkanContext(true)

  test("Memory barrier"):
    val code = Shader.loadShader("copy_test.spv")
    val layout = LayoutInfo(Seq(LayoutSet(0, Seq(Binding(0, InputBufferSize(4)))), LayoutSet(1, Seq(Binding(0, InputBufferSize(4))))))
    val shader = new Shader(code, new org.joml.Vector3i(128, 1, 1), layout, "main", vulkanContext.device)
    val copy1 = new ComputePipeline(shader, vulkanContext)
    val copy2 = new ComputePipeline(shader, vulkanContext)

    val sequence = ComputationSequence(
      Seq(Compute(copy1, Map(LayoutLocation(0, 0) -> LoadTo)), Compute(copy2, Map(LayoutLocation(1, 0) -> LoadFrom))),
      Seq(Dependency(copy1, 1, copy2, 0))
    )
    val sequenceExecutor = new SequenceExecutor(sequence, vulkanContext)
    val input = 0 until 1024
    
    val inputBuffer = new Buffer(
      input.length * 4, // 4 bytes per int
      VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      VMA_MEMORY_USAGE_CPU_ONLY,
      vulkanContext.allocator
    )
    
    val mappedBuffer = inputBuffer.map()
    input.foreach(mappedBuffer.putInt)
    inputBuffer.unmap()
    
    val res = sequenceExecutor.execute(Seq(inputBuffer), input.length)
    
    val outputMappedBuffer = res.head.map()
    val output = (0 until input.length).map(_ => outputMappedBuffer.getInt)
    res.head.unmap()

    assertEquals(input.map(_ + 20000).toList, output.toList)
    
    // Clean up
    inputBuffer.destroy()
    res.foreach(_.destroy())
    sequenceExecutor.destroy()
    copy1.destroy()
    copy2.destroy()
    shader.destroy()