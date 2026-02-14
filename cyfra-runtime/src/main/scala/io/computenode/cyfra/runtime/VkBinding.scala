package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.Provenance
import io.computenode.cyfra.dsl.*
import izumi.reflect.Tag
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.core.Queue
import io.computenode.cyfra.vulkan.core.Device
import izumi.reflect.Tag
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.{VK_BUFFER_USAGE_STORAGE_BUFFER_BIT, VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_BUFFER_USAGE_TRANSFER_SRC_BIT}
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import izumi.reflect.Tag
import org.lwjgl.vulkan.VK10
import org.lwjgl.vulkan.VK10.*
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform, Provenance as ProvenanceBase}
import scala.collection.mutable
import scala.util.chaining.given

/** Vulkan-backed binding with underlying GPU buffer. */
sealed abstract class VkBinding[T <: Value: {Tag, FromExpr}](
  val buffer: Buffer,
  val provenance: ProvenanceBase,
) extends GBinding[T]:
  val sizeOfT: Int = typeStride(summon[Tag[T]])

object VkBinding:
  def unapply(binding: GBinding[?]): Option[Buffer] = binding match
    case b: VkBinding[?] => Some(b.buffer)
    case _               => None

class VkBuffer[T <: Value: {Tag, FromExpr}] private (
  val length: Int,
  underlying: Buffer,
  override val provenance: ProvenanceBase,
) extends VkBinding(underlying, provenance)
    with GBuffer[T]:

  override def withProvenance(p: ProvenanceBase): GBuffer[T] =
    new VkBuffer[T](length, underlying, p)

object VkBuffer:
  private final val Padding = 64
  private final val UsageFlags = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT

  def apply[T <: Value: {Tag, FromExpr}](name: String, length: Int)(using Allocator): VkBuffer[T] =
    val sizeOfT = typeStride(summon[Tag[T]])
    val size = (length * sizeOfT + Padding - 1) / Padding * Padding
    val buffer = new Buffer.DeviceBuffer(size, UsageFlags)
    new VkBuffer[T](length, buffer, Provenance.External(name))

class VkUniform[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}] private (
  underlying: Buffer,
  override val provenance: ProvenanceBase,
) extends VkBinding[T](underlying, provenance)
    with GUniform[T]:

  override def withProvenance(p: ProvenanceBase): GUniform[T] =
    new VkUniform[T](underlying, p)

object VkUniform:
  private final val UsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT |
    VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT

  def apply[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](name: String)(using Allocator): VkUniform[T] =
    val schema = summon[GStructSchema[T]]
    val sizeOfT = totalStride(schema)
    // Use DeviceBuffer for uniforms - GPU-optimal memory for fast shader access
    val buffer = new Buffer.DeviceBuffer(sizeOfT, UsageFlags)
    new VkUniform[T](buffer, Provenance.External(name))
