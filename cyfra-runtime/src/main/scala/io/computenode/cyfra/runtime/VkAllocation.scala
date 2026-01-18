package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.{Allocation, GCodec, GExecution, GProgram}
import io.computenode.cyfra.core.expression.{Expression, Int32, Value, typeStride}
import io.computenode.cyfra.core.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.runtime.VkAllocation.getUnderlying
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.{VK10, VkCommandBuffer, VkCommandBufferBeginInfo, VkDependencyInfo, VkMemoryBarrier2}
import org.lwjgl.vulkan.VK13.*
import org.lwjgl.vulkan.VK10.*

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try
import scala.util.chaining.*

class VkAllocation(val commandPool: CommandPool.Reset, executionHandler: ExecutionHandler)(using Allocator, Device) extends Allocation:
  given VkAllocation = this

  override def submitLayout[L: Layout](layout: L): Unit =
    val executions = Layout[L]
      .toBindings(layout)
      .flatMap(x => Try(getUnderlying(x)).toOption)
      .flatMap(_.execution.fold(Seq(_), _.toSeq))
      .filter(_.isPending)

    PendingExecution.executeAll(executions, this)

  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0): Unit =
      val size = bb.remaining()
      buffer match
        case VkBinding(buffer: Buffer.HostBuffer) => buffer.copyTo(bb, offset)
        case binding: VkBinding[?]                =>
          binding.materialise(this)
          val stagingBuffer = getStagingBuffer(size)
          Buffer.copyBuffer(binding.buffer, stagingBuffer, offset, 0, size, commandPool)
          stagingBuffer.copyTo(bb, 0)
          stagingBuffer.destroy()
        case _ => throw new IllegalArgumentException(s"Tried to read from non-VkBinding $buffer")

    def write(bb: ByteBuffer, offset: Int = 0): Unit =
      val size = bb.remaining()
      buffer match
        case VkBinding(buffer: Buffer.HostBuffer) => buffer.copyFrom(bb, offset)
        case binding: VkBinding[?]                =>
          binding.materialise(this)
          val stagingBuffer = getStagingBuffer(size)
          stagingBuffer.copyFrom(bb, 0)
          val cb = Buffer.copyBufferCommandBuffer(stagingBuffer, binding.buffer, 0, offset, size, commandPool)
          val cleanup = () =>
            commandPool.freeCommandBuffer(cb)
            stagingBuffer.destroy()
          val pe = new PendingExecution(cb, binding.execution.fold(Seq(_), _.toSeq), cleanup)
          addExecution(pe)
          binding.execution = Left(pe)
        case _ => throw new IllegalArgumentException(s"Tried to write to non-VkBinding $buffer")

  extension [T: Value](buffer: GBinding[T])

    def writeArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Unit =
      val bb = BufferUtils.createByteBuffer(arr.size * typeStride(Value[T]))
      buffer.write(bb, 0)
      GCodec.toByteBuffer[T, ST](bb, arr)

    def readArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Array[ST] =
      val bb = BufferUtils.createByteBuffer(arr.size * typeStride(Value[T]))
      buffer.read(bb, 0)
      GCodec.fromByteBuffer[T, ST](bb, arr)

  extension (buffers: GBuffer.type)
    def apply[T: Value](length: Int): GBuffer[T] =
      VkBuffer[T](length).tap(bindings += _)

    def apply[ST: ClassTag, T: Value](scalaArray: Array[ST])(using GCodec[T, ST]): GBuffer[T] =
      val bb = BufferUtils.createByteBuffer(scalaArray.size * typeStride(Value[T]))
      GCodec.toByteBuffer[T, ST](bb, scalaArray)
      GBuffer[T](bb)

    def apply[T: Value](buff: ByteBuffer): GBuffer[T] =
      val sizeOfT = typeStride(Value[T])
      val length = buff.capacity() / sizeOfT
      if buff.capacity() % sizeOfT != 0 then
        throw new IllegalArgumentException(s"ByteBuffer size ${buff.capacity()} is not a multiple of element size $sizeOfT")
      GBuffer[T](length).tap(_.write(buff))

  extension (uniforms: GUniform.type)
    def apply[T: Value](buff: ByteBuffer): GUniform[T] =
      GUniform[T]().tap(_.write(buff))

    def apply[ST: ClassTag, T: Value](value: ST)(using GCodec[T, ST]): GUniform[T] =
      val bb = BufferUtils.createByteBuffer(typeStride(Value[T]))
      GCodec.toByteBuffer[T, ST](bb, Array(value))
      GUniform[T](bb)

    def apply[T: Value](): GUniform[T] =
      VkUniform[T]().tap(bindings += _)

  extension [Params, EL: Layout, RL: Layout](execution: GExecution[Params, EL, RL])
    def execute(params: Params, layout: EL): RL =
      executionHandler.handle(execution, params, layout)

  private def direct[T: Value](buff: ByteBuffer): GUniform[T] =
    GUniform[T](buff)
  def getInitProgramLayout: GProgram.InitProgramLayout =
    new GProgram.InitProgramLayout:
      extension (uniforms: GUniform.type)
        def apply[T: Value](value: T): GUniform[T] = pushStack: stack =>
          val exp = Value[T].peel(value)
          val bb = exp.result match
            case x: Expression.Constant[Int32] => MemoryUtil.memByteBuffer(stack.ints(x.value.asInstanceOf[Int]))
            case _                             => ???
          direct(bb)

  private val executions = mutable.Buffer[PendingExecution]()

  def addExecution(pe: PendingExecution): Unit =
    executions += pe

  private val bindings = mutable.Buffer[VkUniform[?] | VkBuffer[?]]()
  private[cyfra] def close(): Unit =
    executions.filter(_.isRunning).foreach(_.block())
    executions.foreach(_.destroy())
    bindings.map(getUnderlying).foreach(_.buffer.destroy())

  private def getStagingBuffer(size: Int): Buffer.HostBuffer =
    Buffer.HostBuffer(size, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT)

  lazy val synchroniseCommand: VkCommandBuffer = pushStack: stack =>
    val commandBuffer = commandPool.createCommandBuffer()
    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")
    val memoryBarrier = VkMemoryBarrier2
      .calloc(1, stack)
      .sType$Default()
      .srcStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT | VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT)
      .srcAccessMask(
        VK_ACCESS_2_SHADER_READ_BIT | VK_ACCESS_2_SHADER_WRITE_BIT | VK_ACCESS_TRANSFER_READ_BIT | VK_ACCESS_TRANSFER_WRITE_BIT |
          VK_ACCESS_2_UNIFORM_READ_BIT,
      )
      .dstStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT | VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT)
      .dstAccessMask(
        VK_ACCESS_2_SHADER_READ_BIT | VK_ACCESS_2_SHADER_WRITE_BIT | VK_ACCESS_TRANSFER_READ_BIT | VK_ACCESS_TRANSFER_WRITE_BIT |
          VK_ACCESS_2_UNIFORM_READ_BIT,
      )

    val dependencyInfo = VkDependencyInfo
      .calloc(stack)
      .sType$Default()
      .pMemoryBarriers(memoryBarrier)

    vkCmdPipelineBarrier2(commandBuffer, dependencyInfo)
    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")

    commandBuffer

object VkAllocation:
  private[runtime] def getUnderlying(buffer: GBinding[?]): VkBinding[?] =
    buffer match
      case buffer: VkBinding[?] => buffer
      case _                    => throw new IllegalArgumentException(s"Tried to get underlying of non-VkBinding $buffer")
