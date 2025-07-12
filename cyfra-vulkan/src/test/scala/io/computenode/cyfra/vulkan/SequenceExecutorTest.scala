package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.vulkan.compute.ComputePipeline.*
import io.computenode.cyfra.vulkan.compute.ComputePipeline.BindingType.StorageBuffer
import io.computenode.cyfra.vulkan.core.Device
import munit.FunSuite
import org.lwjgl.BufferUtils

class SequenceExecutorTest extends FunSuite:
  val vulkanContext = VulkanContext()
  import vulkanContext.given

  test("Memory barrier"):
    val code = ???
    val layout =
      LayoutInfo(Seq(DescriptorSetInfo(Seq(DescriptorInfo(StorageBuffer))), DescriptorSetInfo(Seq(DescriptorInfo(StorageBuffer)))))
    val copy1 = new ComputePipeline(code, "main", layout)
    val copy2 = new ComputePipeline(code, "main", layout)

    val sequence = ???
    val sequenceExecutor = ???
    val input = 0 until 1024
    val buffer = BufferUtils.createByteBuffer(input.length * 4)
    input.foreach(buffer.putInt)
    buffer.flip()
    val res = ???
    val output = input.map(_ => ???)

    assertEquals(input.map(_ + 20000).toList, output.toList)
