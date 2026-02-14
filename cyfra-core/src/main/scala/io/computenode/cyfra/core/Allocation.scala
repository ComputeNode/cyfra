package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag

import java.nio.ByteBuffer
import scala.reflect.ClassTag

/** Allocation context for GPU buffer management and execution.
  * Handles buffer creation, data transfer, and computation DAG materialization.
  */
trait Allocation:

  // === Materialization - executes computation DAG ===

  /** Materialize a layout by executing its computation DAG.
    * This traverses the provenance of all bindings in the layout,
    * builds an execution plan, and runs the GPU computations.
    *
    * @return A new layout with all bindings having Materialized provenance
    */
  def materialize[L: Layout](layout: L): L

  // === Buffer operations (renamed to avoid confusion with DSL read/write) ===

  extension (buffer: GBinding[?])
    /** Read buffer contents into a ByteBuffer. Buffer must be materialized. */
    def readTo(bb: ByteBuffer, offset: Int = 0): Unit

    /** Write from a ByteBuffer into the buffer. */
    def writeFrom(bb: ByteBuffer, offset: Int = 0): Unit

  extension [T <: Value: {Tag, FromExpr}](buffer: GBinding[T])
    /** Read buffer contents into an array. Buffer must be materialized. */
    def readArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Array[ST]

    /** Write from an array into the buffer. */
    def writeArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Unit

  // === Buffer creation (all have External provenance) ===

  /** Create a buffer with specified length. */
  def buffer[T <: Value: {Tag, FromExpr}](name: String, length: Int): GBuffer[T]

  /** Create a buffer with specified length (unnamed). */
  def buffer[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T]

  /** Create a buffer from an array. */
  def buffer[ST: ClassTag, T <: Value: {Tag, FromExpr}](name: String, scalaArray: Array[ST])(using GCodec[T, ST]): GBuffer[T]

  /** Create a buffer from an array (unnamed). */
  def buffer[ST: ClassTag, T <: Value: {Tag, FromExpr}](scalaArray: Array[ST])(using GCodec[T, ST]): GBuffer[T]

  /** Create a buffer from a ByteBuffer. */
  def buffer[T <: Value: {Tag, FromExpr}](name: String, buff: ByteBuffer): GBuffer[T]

  /** Create a buffer from a ByteBuffer (unnamed). */
  def buffer[T <: Value: {Tag, FromExpr}](buff: ByteBuffer): GBuffer[T]

  /** Create a uniform from a ByteBuffer. */
  def uniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](name: String, buff: ByteBuffer): GUniform[T]

  /** Create a uniform from a ByteBuffer (unnamed). */
  def uniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](buff: ByteBuffer): GUniform[T]

  /** Create a uniform from a value. */
  def uniform[ST: ClassTag, T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](name: String, value: ST)(using GCodec[T, ST]): GUniform[T]

  /** Create a uniform from a value (unnamed). */
  def uniform[ST: ClassTag, T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](value: ST)(using GCodec[T, ST]): GUniform[T]

  /** Create an empty uniform. */
  def uniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](name: String): GUniform[T]
