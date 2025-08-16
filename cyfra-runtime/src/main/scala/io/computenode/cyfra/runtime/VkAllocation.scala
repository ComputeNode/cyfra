package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding}
import io.computenode.cyfra.core.{Allocation, GExecution, GProgram}
import io.computenode.cyfra.core.SpirvProgram
import io.computenode.cyfra.dsl.Expression.ConstInt32
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.runtime.VkAllocation.getUnderlying
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.util.Util.pushStack
import io.computenode.cyfra.dsl.Value.Int32
import izumi.reflect.Tag
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.{VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_BUFFER_USAGE_TRANSFER_SRC_BIT}

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.util.chaining.*

class VkAllocation(commandPool: CommandPool, executionHandler: ExecutionHandler)(using Allocator) extends Allocation:
  given VkAllocation = this

  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0): Unit =
      val size = bb.remaining()
      getUnderlying(buffer) match
        case buffer: Buffer.HostBuffer   => buffer.copyTo(bb, offset)
        case buffer: Buffer.DeviceBuffer =>
          val stagingBuffer = getStagingBuffer(size)
          Buffer.copyBuffer(buffer, stagingBuffer, offset, 0, size, commandPool)
          stagingBuffer.copyTo(bb, 0)

    def write(bb: ByteBuffer, offset: Int = 0): Unit =
      val size = bb.remaining()
      getUnderlying(buffer) match
        case buffer: Buffer.HostBuffer   => buffer.copyFrom(bb, offset)
        case buffer: Buffer.DeviceBuffer =>
          val stagingBuffer = getStagingBuffer(size)
          stagingBuffer.copyFrom(bb, offset)
          Buffer.copyBuffer(stagingBuffer, buffer, 0, offset, size, commandPool)

  extension (buffers: GBuffer.type)
    def apply[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T] =
      VkBuffer[T](length).tap(bindings += _)

    def apply[T <: Value: {Tag, FromExpr}](buff: ByteBuffer): GBuffer[T] =
      val sizeOfT = typeStride(summon[Tag[T]])
      val length = buff.remaining() / sizeOfT
      if buff.remaining() % sizeOfT != 0 then ???
      GBuffer[T](length).tap(_.write(buff))

  extension (buffers: GUniform.type)
    def apply[T <: Value: {Tag, FromExpr}](buff: ByteBuffer): GUniform[T] =
      GUniform[T]().tap(_.write(buff))

    def apply[T <: Value: {Tag, FromExpr}](): GUniform[T] =
      VkUniform[T]().tap(bindings += _)

  extension [Params, EL <: Layout: LayoutBinding, RL <: Layout: LayoutBinding](execution: GExecution[Params, EL, RL])
    def execute(params: Params, layout: EL): RL = executionHandler.handle(execution, params, layout)

  private def direct[T <: Value: {Tag, FromExpr}](buff: ByteBuffer): GUniform[T] =
    GUniform[T](buff)

  def getInitProgramLayout: GProgram.InitProgramLayout =
    new GProgram.InitProgramLayout:
      extension (uniforms: GUniform.type)
        def apply[T <: GStruct[T]: {Tag, FromExpr}](value: T): GUniform[T] = pushStack: stack =>
          val bb = value.productElement(0) match
            case Int32(tree: ConstInt32) => MemoryUtil.memByteBuffer(stack.ints(tree.value))
            case _                       => ???
          direct(bb)

  private val bindings = mutable.Buffer[VkUniform[?] | VkBuffer[?]]()
  private[cyfra] def close(): Unit =
    bindings.map(getUnderlying).foreach(_.destroy())
    stagingBuffer.foreach(_.destroy())

  private var stagingBuffer: Option[Buffer.HostBuffer] = None
  private def getStagingBuffer(size: Int): Buffer.HostBuffer =
    stagingBuffer match
      case Some(buffer) if buffer.size >= size => buffer
      case _                                   =>
        stagingBuffer.foreach(_.destroy())
        val newBuffer = Buffer.HostBuffer(size, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
        stagingBuffer = Some(newBuffer)
        newBuffer

object VkAllocation:
  private[runtime] def getUnderlying(buffer: GBinding[?]): Buffer =
    buffer match
      case buffer: VkBuffer[?]   => buffer.underlying
      case uniform: VkUniform[?] => uniform.underlying
      case _                     => throw new IllegalArgumentException(s"Tried to get underlying of non-VkBinding $buffer")
