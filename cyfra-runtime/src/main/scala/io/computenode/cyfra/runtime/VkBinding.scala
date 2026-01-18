package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.core.expression.typeStride
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.core.{Device, Queue}
import izumi.reflect.Tag
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.*
import io.computenode.cyfra.core.binding.{GBinding, GBuffer, GUniform}
import scala.collection.mutable
import scala.util.chaining.given

sealed abstract class VkBinding[T : Value](val buffer: Buffer):
  val sizeOfT: Int = typeStride(Value[T])

  /** Holds either:
    *   1. a single execution that writes to this buffer
    *   1. multiple executions that read from this buffer
    */
  var execution: Either[PendingExecution, mutable.Buffer[PendingExecution]] = Right(mutable.Buffer.empty)

  def materialise(allocation: VkAllocation)(using Device): Unit =
    val allExecs = execution.fold(Seq(_), _.toSeq) // TODO better handle read only executions
    allExecs.filter(_.isPending).pipe(PendingExecution.executeAll(_, allocation))
    allExecs.foreach(_.block())
    PendingExecution.cleanupAll(allExecs)

object VkBinding:
  def unapply(binding: GBinding[?]): Option[Buffer] = binding match
    case b: VkBinding[?] => Some(b.buffer)
    case _               => None

class VkBuffer[T : Value] private (val length: Int, underlying: Buffer) extends VkBinding(underlying) with GBuffer[T]

object VkBuffer:
  private final val Padding = 64
  private final val UsageFlags = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT

  def apply[T : Value](length: Int)(using Allocator): VkBuffer[T] =
    val sizeOfT = typeStride(Value[T])
    val size = (length * sizeOfT + Padding - 1) / Padding * Padding
    val buffer = new Buffer.DeviceBuffer(size, UsageFlags)
    new VkBuffer[T](length, buffer)

class VkUniform[T : Value] private (underlying: Buffer) extends VkBinding[T](underlying) with GUniform[T]

object VkUniform:
  private final val UsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT |
    VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT

  def apply[T : Value]()(using Allocator): VkUniform[T] =
    val sizeOfT = typeStride(Value[T])
    val buffer = new Buffer.DeviceBuffer(sizeOfT, UsageFlags)
    new VkUniform[T](buffer)
