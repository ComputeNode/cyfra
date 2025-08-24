package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import izumi.reflect.Tag
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer}
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.core.Queue
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.{VK_BUFFER_USAGE_STORAGE_BUFFER_BIT, VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_BUFFER_USAGE_TRANSFER_SRC_BIT}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GUniform
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import izumi.reflect.Tag
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.*

import scala.collection.mutable

sealed abstract class VkBinding[T <: Value: {Tag, FromExpr}](val buffer: Buffer):
  val sizeOfT: Int = typeStride(summon[Tag[T]])

  /** Holds either:
    *   1. a single execution that writes to this buffer
    *   1. multiple executions that read from this buffer
    */
  var execution: Either[PendingExecution, mutable.Buffer[PendingExecution]] = Right(mutable.Buffer.empty)

  def materialise(queue: Queue)(using Device): Unit = execution match
    case Left(exec) if exec.isAlive =>
      PendingExecution.executeAll(Seq(exec), queue)
      exec.block()
      PendingExecution.cleanupAll(Seq(exec))
    case _ => ()

object VkBinding:
  def unapply(binding: GBinding[?]): Option[Buffer] = binding match
    case b: VkBinding[?] => Some(b.buffer)
    case _               => None

class VkBuffer[T <: Value: {Tag, FromExpr}] private (val length: Int, underlying: Buffer) extends VkBinding(underlying) with GBuffer[T]

object VkBuffer:
  private final val Padding = 64
  private final val UsageFlags = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT

  def apply[T <: Value: {Tag, FromExpr}](length: Int)(using Allocator): VkBuffer[T] =
    val sizeOfT = typeStride(summon[Tag[T]])
    val size = (length * sizeOfT + Padding - 1) / Padding * Padding
    val buffer = new Buffer.DeviceBuffer(size, UsageFlags)
    new VkBuffer[T](length, buffer)

class VkUniform[T <: Value: {Tag, FromExpr}] private (underlying: Buffer) extends VkBinding[T](underlying) with GUniform[T]

object VkUniform:
  private final val UsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT |
    VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT

  def apply[T <: Value: {Tag, FromExpr}]()(using Allocator): VkUniform[T] =
    val sizeOfT = typeStride(summon[Tag[T]])
    val buffer = new Buffer.DeviceBuffer(sizeOfT, UsageFlags)
    new VkUniform[T](buffer)
