package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.core.{Allocation, GExecution, GProgram}
import io.computenode.cyfra.core.SpirvProgram
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import izumi.reflect.Tag
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.{VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_BUFFER_USAGE_TRANSFER_SRC_BIT}

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.util.chaining.*

class VkAllocation(commandPool: CommandPool, executionHandler: ExecutionHandler)(using Allocator) extends Allocation:
  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0, size: Int = -1): Unit =
      val buf = getUnderlying(buffer)
      val s = if size < 0 then buf.size - offset else size

      buf match
        case buffer: Buffer.HostBuffer   => Buffer.copyBuffer(buffer, bb, offset, 0, s)
        case buffer: Buffer.DeviceBuffer =>
          val stagingBuffer = getStagingBuffer(s)
          Buffer.copyBuffer(buffer, stagingBuffer, offset, 0, s, commandPool).block().destroy()
          Buffer.copyBuffer(stagingBuffer, bb, 0, 0, s)

    def write(bb: ByteBuffer, offset: Int = 0, size: Int = -1): Unit =
      val buf = getUnderlying(buffer)
      val s = if size < 0 then bb.remaining() else size

      buf match
        case buffer: Buffer.HostBuffer   => Buffer.copyBuffer(bb, buffer, offset, 0, s)
        case buffer: Buffer.DeviceBuffer =>
          val stagingBuffer = getStagingBuffer(s)
          Buffer.copyBuffer(bb, stagingBuffer, 0, 0, s)
          Buffer.copyBuffer(stagingBuffer, buffer, 0, offset, s, commandPool).block().destroy()

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

  extension [Params, L <: Layout, RL <: Layout: LayoutStruct](execution: GExecution[Params, L, RL])
    override def execute(params: Params, layout: L): RL = executionHandler.handle(execution, params, layout)

  private def getUnderlying(buffer: GBinding[?]): Buffer =
    buffer match
      case buffer: VkBuffer[?]   => buffer.underlying
      case uniform: VkUniform[?] => uniform.underlying
      case _                     => ???

  private val bindings = mutable.Buffer[VkUniform[?] | VkBuffer[?]]()
  private[cyfra] def close(): Unit = bindings.map(getUnderlying).foreach(_.destroy())

  private var stagingBuffer: Option[Buffer.HostBuffer] = None
  private def getStagingBuffer(size: Int): Buffer.HostBuffer =
    stagingBuffer match
      case Some(buffer) if buffer.size >= size => buffer
      case _                                   =>
        stagingBuffer.foreach(_.destroy())
        val newBuffer = Buffer.HostBuffer(size, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
        stagingBuffer = Some(newBuffer)
        newBuffer
