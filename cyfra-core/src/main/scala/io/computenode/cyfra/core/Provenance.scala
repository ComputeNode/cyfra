package io.computenode.cyfra.core

import io.computenode.cyfra.dsl.binding.{GBinding, Provenance as ProvenanceBase}
import java.util.UUID

/** Concrete provenance implementations for DAG-based execution. */
object Provenance:

  /** Binding provided externally by the user. */
  case class External(name: String) extends ProvenanceBase

  /** Binding produced by dispatching a GProgram.
    * @param program Reference to the GProgram that produced this binding
    * @param params The params used for this dispatch (stored as Any due to type erasure)
    * @param inputs Input bindings that were passed to the dispatch
    * @param outputIndex Position of this binding in the program's output layout
    */
  case class ExecutionNode(
    program: GProgram[?, ?],
    params: Any,
    inputs: Seq[GBinding[?]],
    outputIndex: Int,
  ) extends ProvenanceBase

  /** Binding created by copying (partial or full) from another binding. */
  case class Copied(
    source: GBinding[?],
    readIndex: Int,
    writeIndex: Int,
    size: Int,
  ) extends ProvenanceBase

  /** Binding that has been materialized - lineage is no longer tracked.
    * The data is now available in the runtime.
    * @param executionId Unique ID from the runtime's materialization
    * @param layoutIndex Position of this binding in the materialized layout
    */
  case class Materialized(executionId: UUID, layoutIndex: Int) extends ProvenanceBase
