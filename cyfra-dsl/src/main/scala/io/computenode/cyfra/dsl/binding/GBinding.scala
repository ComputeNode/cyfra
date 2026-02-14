package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr as fromExprEval
import io.computenode.cyfra.dsl.Value.{FloatType, FromExpr, Int32, Vec4}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

/** Base trait for tracking where a binding came from (DAG lineage).
  * Concrete implementations are in cyfra-core.
  */
trait Provenance

/** Marker for bindings that have no provenance tracking (DSL-only use). */
object NoProvenance extends Provenance

trait GBinding[T <: Value: {Tag, FromExpr}]:
  def tag = summon[Tag[T]]
  def fromExpr = summon[FromExpr[T]]

  /** The provenance tracks where this binding came from for DAG execution.
    * NoProvenance for DSL-only bindings.
    */
  def provenance: Provenance

  /** Create a copy of this binding with new provenance. */
  def withProvenance(p: Provenance): GBinding[T]

trait GBuffer[T <: Value: {FromExpr, Tag}] extends GBinding[T]:
  def read(index: Int32): T = FromExpr.fromExpr(ReadBuffer(this, index))

  def write(index: Int32, value: T): GIO[Empty] = GIO.write(this, index, value)

  /** Create a copy with new provenance. */
  override def withProvenance(p: Provenance): GBuffer[T]

object GBuffer:
  /** Extension to read 4 consecutive elements as Vec4 from a float buffer.
    * @param buffer The buffer to read from
    * @param index Base index - reads elements at index, index+1, index+2, index+3
    * @return Vec4 containing the 4 consecutive values
    */
  extension [T <: FloatType: Tag: FromExpr](buffer: GBuffer[T])
    def readVec4(index: Int32)(using Tag[Vec4[T]]): Vec4[T] = 
      FromExpr.fromExpr[Vec4[T]](ReadBufferVec4(buffer, index))

  /** Standard implementation of GBuffer with provenance tracking. */
  final class Impl[T <: Value: {FromExpr, Tag}](
    val provenance: Provenance = NoProvenance,
  ) extends GBuffer[T]:
    override def withProvenance(p: Provenance): GBuffer[T] = new Impl[T](p)

  /** Create a buffer with no provenance (DSL-only). */
  def apply[T <: Value: {FromExpr, Tag}](): GBuffer[T] =
    new Impl[T](NoProvenance)

  /** Create a buffer with specific provenance. */
  def apply[T <: Value: {FromExpr, Tag}](provenance: Provenance): GBuffer[T] =
    new Impl[T](provenance)

trait GUniform[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}] extends GBinding[T]:
  def read: T = fromExprEval(ReadUniform(this))

  def write(value: T): GIO[Empty] = WriteUniform(this, value)

  def schema = summon[GStructSchema[T]]

  /** Create a copy with new provenance. */
  override def withProvenance(p: Provenance): GUniform[T]

object GUniform:

  /** Standard implementation of GUniform with provenance tracking. */
  final class Impl[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](
    val provenance: Provenance = NoProvenance,
  ) extends GUniform[T]:
    override def withProvenance(p: Provenance): GUniform[T] = new Impl[T](p)

  /** Create a uniform with no provenance (DSL-only). */
  def apply[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](): GUniform[T] =
    new Impl[T](NoProvenance)

  /** Create a uniform with specific provenance. */
  def apply[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](provenance: Provenance): GUniform[T] =
    new Impl[T](provenance)

  class ParamUniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}]() extends GUniform[T]:
    val provenance: Provenance = NoProvenance
    override def withProvenance(p: Provenance): GUniform[T] = new Impl[T](p)

  def fromParams[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}] = ParamUniform[T]()
