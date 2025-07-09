package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.vulkan.compute.ComputePipeline.*
import io.computenode.cyfra.vulkan.compute.ComputePipeline.BindingType.StorageBuffer
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.BufferAction.{LoadFrom, LoadTo}
import io.computenode.cyfra.vulkan.executor.SequenceExecutor
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.{ComputationSequence, Compute, Dependency, LayoutLocation}
import munit.FunSuite
import org.lwjgl.BufferUtils

class SequenceExecutorTest extends FunSuite:
  val vulkanContext = VulkanContext()
  import vulkanContext.given

  test("Memory barrier"):
    val code = ComputePipeline.loadShader("copy_test.spv").get
    val layout =
      LayoutInfo(Seq(DescriptorSetInfo(Seq(DescriptorInfo(StorageBuffer))), DescriptorSetInfo(Seq(DescriptorInfo(StorageBuffer)))))
    val copy1 = new ComputePipeline(code, "main", layout)
    val copy2 = new ComputePipeline(code, "main", layout)

    val sequence =
      ComputationSequence(
        Seq(Compute(copy1, Map(LayoutLocation(0, 0) -> LoadTo)), Compute(copy2, Map(LayoutLocation(1, 0) -> LoadFrom))),
        Seq(Dependency(copy1, 1, copy2, 0)),
      )
    val sequenceExecutor = new SequenceExecutor(sequence, vulkanContext)
    val input = 0 until 1024
    val buffer = BufferUtils.createByteBuffer(input.length * 4)
    input.foreach(buffer.putInt)
    buffer.flip()
    val res = sequenceExecutor.execute(Seq(buffer))
    val output = input.map(_ => res.head.getInt)

    assertEquals(input.map(_ + 20000).toList, output.toList)
