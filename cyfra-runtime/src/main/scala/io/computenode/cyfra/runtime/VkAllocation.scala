package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding}
import io.computenode.cyfra.core.{Allocation, GExecution, GProgram}
import io.computenode.cyfra.core.SpirvProgram
import io.computenode.cyfra.dsl.Expression.ConstInt32
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.runtime.VkAllocation.getUnderlying
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.util.Util.pushStack
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK13.VK_PIPELINE_STAGE_2_COPY_BIT
import org.lwjgl.vulkan.VK10.{VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_BUFFER_USAGE_TRANSFER_SRC_BIT}

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.util.chaining.*
import scala.util.Try

class VkAllocation(commandPool: CommandPool, executionHandler: ExecutionHandler)(using Allocator, Device) extends Allocation:
  given VkAllocation = this

  override def submitLayout[L <: Layout: LayoutBinding](layout: L): Unit =
    val executions = summon[LayoutBinding[L]]
      .toBindings(layout)
      .flatMap(x => Try(getUnderlying(x)).toOption)
      .flatMap(_.execution.fold(Seq(_), _.toSeq))
      .filter(_.isPending)

    PendingExecution.executeAll(executions, commandPool.queue)

  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0): Unit =
      val size = bb.remaining()
      buffer match
        case VkBinding(buffer: Buffer.HostBuffer) => buffer.copyTo(bb, offset)
        case binding: VkBinding[?]                =>
          binding.materialise(commandPool.queue)
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
          binding.materialise(commandPool.queue)
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

  extension (buffers: GBuffer.type)
    def apply[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T] =
      VkBuffer[T](length).tap(bindings += _)

    def apply[T <: Value: {Tag, FromExpr}](buff: ByteBuffer): GBuffer[T] =
      val sizeOfT = typeStride(summon[Tag[T]])
      val length = buff.capacity() / sizeOfT
      if buff.capacity() % sizeOfT != 0 then
        throw new IllegalArgumentException(s"ByteBuffer size ${buff.capacity()} is not a multiple of element size $sizeOfT")
      GBuffer[T](length).tap(_.write(buff))

  extension (uniforms: GUniform.type)
    def apply[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](buff: ByteBuffer): GUniform[T] =
      GUniform[T]().tap(_.write(buff))

    def apply[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](): GUniform[T] =
      VkUniform[T]().tap(bindings += _)

  extension [Params, EL <: Layout: LayoutBinding, RL <: Layout: LayoutBinding](execution: GExecution[Params, EL, RL])
    def execute(params: Params, layout: EL): RL = executionHandler.handle(execution, params, layout)

  private def direct[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](buff: ByteBuffer): GUniform[T] =
    GUniform[T](buff)
  def getInitProgramLayout: GProgram.InitProgramLayout =
    new GProgram.InitProgramLayout:
      extension (uniforms: GUniform.type)
        def apply[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](value: T): GUniform[T] = pushStack: stack =>
          val bb = value.productElement(0) match
            case Int32(tree: ConstInt32) => MemoryUtil.memByteBuffer(stack.ints(tree.value))
            case _                       => ???
          direct(bb)

  private val executions = mutable.Buffer[PendingExecution]()

  def addExecution(pe: PendingExecution): Unit =
    executions += pe

  private val bindings = mutable.Buffer[VkUniform[?] | VkBuffer[?]]()
  private[cyfra] def close(): Unit =
    executions.foreach(_.destroy())
    bindings.map(getUnderlying).foreach(_.buffer.destroy())

  private def getStagingBuffer(size: Int): Buffer.HostBuffer =
    Buffer.HostBuffer(size, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT)

object VkAllocation:
  private[runtime] def getUnderlying(buffer: GBinding[?]): VkBinding[?] =
    buffer match
      case buffer: VkBinding[?] => buffer
      case _                    => throw new IllegalArgumentException(s"Tried to get underlying of non-VkBinding $buffer")
