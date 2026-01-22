package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

/**
 * Represents a workgroup-local shared memory array.
 * 
 * Shared memory is visible to all invocations within a workgroup and can be used
 * for efficient inter-thread communication within a workgroup after synchronization
 * with [[GIO.barrier]].
 *
 * @tparam T Element type of the shared memory array
 */
trait GShared[T <: Value: {FromExpr, Tag}]:
  def tag: Tag[T] = summon[Tag[T]]
  def size: Int

  /** Read a value from shared memory at the given index. */
  def read(index: Int32): T = fromExpr(ReadShared(this, index))

  /** Write a value to shared memory at the given index. */
  def write(index: Int32, value: T): GIO[Empty] = WriteShared(this, index, value)

object GShared:
  private var nextId = 0

  /** Create a shared memory array with the given size. */
  def apply[T <: Value: {FromExpr, Tag}](size: Int): GShared[T] =
    val id = nextId
    nextId += 1
    new GSharedImpl[T](id, size)

  private[cyfra] class GSharedImpl[T <: Value: {FromExpr, Tag}](val sharedId: Int, val size: Int) extends GShared[T]
