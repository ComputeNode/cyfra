package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.{Allocation, GProgram}
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.runtime.VkAllocation.getUnderlying
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.{VK10, VkCommandBuffer, VkCommandBufferBeginInfo, VkDependencyInfo, VkMemoryBarrier2}
import org.lwjgl.vulkan.VK13.*
import org.lwjgl.vulkan.VK10.*
import java.nio.ByteBuffer
import scala.collection.mutable
import scala.util.Try
import scala.util.chaining.*
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform, Provenance as ProvenanceBase}
import io.computenode.cyfra.core.Provenance
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import scala.reflect.ClassTag
import io.computenode.cyfra.core.GCodec
import io.computenode.cyfra.utility.NVTX

import java.util.UUID

class VkAllocation(val commandPool: CommandPool.Reset, val executionHandler: ExecutionHandler)(using Allocator, Device) extends Allocation:
  given VkAllocation = this

  // === Materialization ===

  override def materialize[L: Layout](layout: L): L =
    val layoutInstance = Layout[L]
    val bindings = layoutInstance.toBindings(layout)

    // Execute the plan via ExecutionHandler (builds DAG, analyzes hazards, single cmd buffer)
    val executionId = UUID.randomUUID()
    executionHandler.executePlan(bindings)

    // Sync all GPU work
    executionHandler.sync()

    // Create materialized bindings
    val materializedBindings = bindings.zipWithIndex.map { case (binding, idx) =>
      binding.withProvenance(Provenance.Materialized(executionId, idx))
    }

    layoutInstance.fromBindings(materializedBindings)
  
  /** Fast path: submit cached execution for a layout WITHOUT rebuilding provenance.
    * Call this instead of dispatch+materialize when you know the buffers haven't changed.
    * Returns true if cache hit, false if you need to fall back to dispatch+materialize.
    */
  def submitCached[L: Layout](layout: L): Boolean =
    val bindings = Layout[L].toBindings(layout)
    executionHandler.submitIfCached(bindings)

  // === Buffer operations (renamed) ===

  extension (buffer: GBinding[?])
    def readTo(bb: ByteBuffer, offset: Int = 0): Unit =
      val size = bb.remaining()
      buffer match
        case VkBinding(buffer: Buffer.HostBuffer) => buffer.copyTo(bb, offset)
        case binding: VkBinding[?]                =>
          NVTX.push(s"Sync[$buffer]")
          executionHandler.sync()
          NVTX.pop()
          NVTX.push(s"CopyToStaging[$buffer]")
          val stagingBuffer = getStagingBuffer(size)
          Buffer.copyBuffer(binding.buffer, stagingBuffer, offset, 0, size, commandPool)
          NVTX.pop()
          NVTX.push(s"CopyToHost[$buffer]")
          stagingBuffer.copyTo(bb, 0)
          NVTX.pop()
          stagingBuffer.destroy()
        case _ => throw new IllegalArgumentException(s"Tried to read from non-VkBinding $buffer")

    def writeFrom(bb: ByteBuffer, offset: Int = 0): Unit =
      val size = bb.remaining()
      buffer match
        case VkBinding(buffer: Buffer.HostBuffer) => buffer.copyFrom(bb, offset)
        case binding: VkBinding[?]                =>
          executionHandler.sync()
          val stagingBuffer = getStagingBuffer(size)
          stagingBuffer.copyFrom(bb, 0)
          Buffer.copyBuffer(stagingBuffer, binding.buffer, 0, offset, size, commandPool)
            stagingBuffer.destroy()
        case _ => throw new IllegalArgumentException(s"Tried to write to non-VkBinding $buffer")

  extension [T <: Value: {Tag, FromExpr}](buffer: GBinding[T])

    def writeArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Unit =
      val bb = BufferUtils.createByteBuffer(arr.size * typeStride(summon[Tag[T]]))
      GCodec.toByteBuffer[T, ST](bb, arr)
      bb.rewind()
      buffer.writeFrom(bb, offset)

    def readArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Array[ST] =
      val bb = BufferUtils.createByteBuffer(arr.size * typeStride(summon[Tag[T]]))
      buffer.readTo(bb, offset)
      bb.rewind()
      GCodec.fromByteBuffer[T, ST](bb, arr)

  // === Buffer creation ===

  // === Buffer creation methods (implement Allocation trait) ===

  override def buffer[T <: Value: {Tag, FromExpr}](name: String, length: Int): GBuffer[T] =
    VkBuffer[T](name, length).tap(bindings += _)

  override def buffer[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T] =
    buffer[T]("buffer", length)

  override def buffer[ST: ClassTag, T <: Value: {Tag, FromExpr}](name: String, scalaArray: Array[ST])(using GCodec[T, ST]): GBuffer[T] =
    val bb = BufferUtils.createByteBuffer(scalaArray.size * typeStride(summon[Tag[T]]))
    GCodec.toByteBuffer[T, ST](bb, scalaArray)
    bb.rewind()
    val buf = buffer[T](name, scalaArray.length)
    buf.writeFrom(bb)
    buf

  override def buffer[ST: ClassTag, T <: Value: {Tag, FromExpr}](scalaArray: Array[ST])(using GCodec[T, ST]): GBuffer[T] =
    buffer[ST, T]("buffer", scalaArray)

  override def buffer[T <: Value: {Tag, FromExpr}](name: String, buff: ByteBuffer): GBuffer[T] =
    val sizeOfT = typeStride(summon[Tag[T]])
    val length = buff.capacity() / sizeOfT
    if buff.capacity() % sizeOfT != 0 then throw new IllegalArgumentException(s"ByteBuffer size ${buff.capacity()} is not a multiple of element size $sizeOfT")
    val buf = buffer[T](name, length)
    buf.writeFrom(buff)
    buf

  override def buffer[T <: Value: {Tag, FromExpr}](buff: ByteBuffer): GBuffer[T] =
    buffer[T]("buffer", buff)

  override def uniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](name: String, buff: ByteBuffer): GUniform[T] =
    val u = VkUniform[T](name)
    bindings += u
    u.writeFrom(buff)
    u

  override def uniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](buff: ByteBuffer): GUniform[T] =
    uniform[T]("uniform", buff)

  override def uniform[ST: ClassTag, T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](name: String, value: ST)(using GCodec[T, ST]): GUniform[T] =
    val bb = BufferUtils.createByteBuffer(totalStride(summon[GStructSchema[T]]))
    GCodec.toByteBuffer[T, ST](bb, Array(value))
    bb.rewind()
    val u = VkUniform[T](name)
    bindings += u
    u.writeFrom(bb)
    u

  override def uniform[ST: ClassTag, T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](value: ST)(using GCodec[T, ST]): GUniform[T] =
    uniform[ST, T]("uniform", value)

  override def uniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](name: String): GUniform[T] =
    VkUniform[T](name).tap(bindings += _)

  def getInitProgramLayout: GProgram.InitProgramLayout =
    new GProgram.InitProgramLayout:
      override def createUniform[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](value: T): GUniform[T] = pushStack: stack =>
        val bb = value.productElement(0) match
          case Int32(tree: ConstInt32) => MemoryUtil.memByteBuffer(stack.ints(tree.value))
          case _                       => ???
        val uniform = VkUniform[T]("param")
        uniform.writeFrom(bb)
        uniform

  private val bindings = mutable.Buffer[VkUniform[?] | VkBuffer[?]]()

  private[cyfra] def close(): Unit =
    executionHandler.sync()
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
