package io.computenode.cyfra.vulkan


import io.computenode.cyfra.vulkan.compute.{Binding, ComputePipeline, InputBufferSize, LayoutInfo, LayoutSet, Shader}
import io.computenode.cyfra.vulkan.executor.BufferAction.{LoadFrom, LoadTo}
import io.computenode.cyfra.vulkan.executor.SequenceExecutor
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.{ComputationSequence, Compute, Dependency, LayoutLocation}
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Test
import org.lwjgl.BufferUtils

class SequenceExecutorTest {
  private val vulkanContext = VulkanContext(true)

  @Test
  def testMemoryBarrier(): Unit = {
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
    val buffer = BufferUtils.createByteBuffer(input.length * 4)
    input.foreach(buffer.putInt)
    buffer.flip()
    val res = sequenceExecutor.execute(Seq(buffer), input.length)
    val output = input.map(_ => res.head.getInt)

    assertArrayEquals(input.map(_ + 20000).toArray, output.toArray)
  }

}
