package io.computenode.cyfra.poc.v3

import java.util.UUID
import scala.quoted.{Expr, Quotes, Type}

// Type alias for F16 (mock - in real impl this would be a proper type)
type Float16 = Float

// =============================================================================
// PROVENANCE - Tracks where a buffer came from (DAG lineage)
// =============================================================================

enum Provenance:
  /** Buffer provided externally by the user */
  case External(name: String)

  /** Buffer produced by executing a GExecution.
    * @param execution Reference to the GExecution that produced this buffer
    * @param inputs Input buffers that were passed to the dispatch
    * @param outputIndex Position of this buffer in the execution's output layout
    */
  case ExecutionNode(
    execution: GExecution[?, ?, ?],
    inputs: Seq[GBuffer[?]],    
    outputIndex: Int,
  )

  /** Buffer created by copying (partial or full) from another buffer */
  case Copied(
    source: GBuffer[?],
    readIndex: Int,
    writeIndex: Int,
    size: Int,
  )

  /** Buffer that has been materialized - lineage is no longer tracked.
    * The data is now available in the runtime's cache.
    * @param executionId Unique ID from the runtime's materialization
    * @param layoutIndex Position of this buffer in the materialized layout
    */
  case Materialized(executionId: UUID, layoutIndex: Int)

// =============================================================================
// GBUFFER - GPU buffer reference with provenance
// =============================================================================

/** A GPU buffer reference. Represents a node in the computation DAG.
  * Does not carry actual data - just the structure of how to compute it.
  * 
  * To read data, first materialize the containing layout via `runtime.materializeLayout(layout)`,
  * then call `readArray()` on the materialized buffer.
  */
final class GBuffer[T](
  val name: String,
  val size: Int,
  val provenance: Provenance,
):
  /** Create a new buffer representing a partial copy from this buffer. */
  def copyTo[S](dst: GBuffer[S], readIndex: Int, writeIndex: Int, copySize: Int): GBuffer[S] =
    new GBuffer[S](
      dst.name,
      dst.size,
      Provenance.Copied(this, readIndex, writeIndex, copySize),
    )

  /** Create a new buffer representing a full copy from this buffer. */
  def copyTo[S](dst: GBuffer[S]): GBuffer[S] =
    new GBuffer[S](
      dst.name,
      dst.size,
      Provenance.Copied(this, 0, 0, size),
    )

  /** Check if this buffer has been materialized. */
  def isMaterialized: Boolean = provenance.isInstanceOf[Provenance.Materialized]

  /** Read the actual data from a materialized buffer.
    * Throws if the buffer is not materialized - use `runtime.materializeLayout()` first.
    */
  def readArray()(using runtime: CyfraRuntime): Array[T] =
    provenance match
      case _: Provenance.Materialized => runtime.readArray(this)
      case _ => throw new IllegalStateException(
        s"Cannot read from non-materialized buffer: $name. " +
        "Use runtime.materializeLayout(layout) first, then read from the resulting buffer."
      )

  /** Read a single value at index from a materialized buffer. */
  def read(index: Int)(using runtime: CyfraRuntime): T =
    readArray()(index)

  override def toString: String = s"GBuffer($name, $size, $provenance)"

object GBuffer:
  /** Create an external buffer (user-provided). */
  def external[T](name: String, size: Int): GBuffer[T] =
    new GBuffer[T](name, size, Provenance.External(name))

// =============================================================================
// GUNIFORM - GPU uniform buffer reference
// =============================================================================

final class GUniform[T](val name: String):
  override def toString: String = s"GUniform($name)"

object GUniform:
  def apply[T](name: String): GUniform[T] = new GUniform(name)

// =============================================================================
// LAYOUT TYPE CLASS - Similar to Cyfra's Layout with fromBindings/toBindings
// =============================================================================

trait Layout[L]:
  /** Extract all buffers from a layout. */
  def toBindings(layout: L): Seq[GBuffer[?]]

  /** Reconstruct a layout from a sequence of buffers. */
  def fromBindings(bindings: Seq[GBuffer[?]]): L

object Layout:
  /** Macro-based derivation for case classes and tuples. */
  inline def derived[L <: Product]: Layout[L] = ${ LayoutMacros.derivedMacro[L] }

  // === Primitive instances ===

  given Layout[EmptyTuple] with
    def toBindings(layout: EmptyTuple) = Seq.empty
    def fromBindings(bindings: Seq[GBuffer[?]]) = EmptyTuple

  given Layout[Unit] with
    def toBindings(layout: Unit) = Seq.empty
    def fromBindings(bindings: Seq[GBuffer[?]]) = ()

  given [T]: Layout[GBuffer[T]] with
    def toBindings(layout: GBuffer[T]) = Seq(layout)
    def fromBindings(bindings: Seq[GBuffer[?]]) = bindings.head.asInstanceOf[GBuffer[T]]

  given [T]: Layout[GUniform[T]] with
    def toBindings(layout: GUniform[T]) = Seq.empty
    def fromBindings(bindings: Seq[GBuffer[?]]) = throw new UnsupportedOperationException("Cannot reconstruct GUniform from bindings")

  // === Tuple instances (recursive) ===

  given tupleConsLayout[H, T <: Tuple](using hLayout: Layout[H], tLayout: Layout[T]): Layout[H *: T] with
    def toBindings(layout: H *: T): Seq[GBuffer[?]] =
      hLayout.toBindings(layout.head) ++ tLayout.toBindings(layout.tail)

    def fromBindings(bindings: Seq[GBuffer[?]]): H *: T =
      val hSize = hLayout.toBindings(null.asInstanceOf[H]).size // hacky but works for GBuffer (size 1)
      val (hBindings, tBindings) = bindings.splitAt(hSize)
      hLayout.fromBindings(hBindings) *: tLayout.fromBindings(tBindings)

  // Fix: for GBuffer the "size" is always 1, so use a cleaner approach
  given tuple2Layout[A, B](using la: Layout[A], lb: Layout[B]): Layout[(A, B)] with
    def toBindings(layout: (A, B)): Seq[GBuffer[?]] =
      la.toBindings(layout._1) ++ lb.toBindings(layout._2)

    def fromBindings(bindings: Seq[GBuffer[?]]): (A, B) =
      // For simplicity, assume each element is a single GBuffer
      val aSize = 1 // GBuffer layouts are size 1
      (la.fromBindings(bindings.take(aSize)), lb.fromBindings(bindings.drop(aSize)))

  given tuple3Layout[A, B, C](using la: Layout[A], lb: Layout[B], lc: Layout[C]): Layout[(A, B, C)] with
    def toBindings(layout: (A, B, C)): Seq[GBuffer[?]] =
      la.toBindings(layout._1) ++ lb.toBindings(layout._2) ++ lc.toBindings(layout._3)

    def fromBindings(bindings: Seq[GBuffer[?]]): (A, B, C) =
      (la.fromBindings(bindings.slice(0, 1)),
       lb.fromBindings(bindings.slice(1, 2)),
       lc.fromBindings(bindings.slice(2, 3)))

  def apply[L](using l: Layout[L]): Layout[L] = l

// =============================================================================
// LAYOUT MACROS - Compile-time derivation for case classes
// =============================================================================

object LayoutMacros:
  def derivedMacro[L <: Product: Type](using quotes: Quotes): Expr[Layout[L]] =
    import quotes.reflect.*

    val layoutType = TypeRepr.of[L]
    val layoutSymbol = layoutType.typeSymbol

    // Check if it's a case class
    val isCaseClass = layoutSymbol.flags.is(Flags.Case)
    // Check if it's a tuple (Tuple1, Tuple2, etc.)
    val isTuple = layoutSymbol.fullName.startsWith("scala.Tuple")

    if !isCaseClass && !isTuple then
      report.errorAndAbort(s"Can only derive Layout for case classes or tuples, found: ${layoutType.show}")

    if isTuple then
      // For tuples, use productElement access
      val typeArgs = layoutType.typeArgs

      // Validate all type args are GBuffer[?]
      typeArgs.zipWithIndex.foreach { (tpe, idx) =>
        tpe match
          case AppliedType(tycon, _) if tycon.typeSymbol.name == "GBuffer" =>
            // OK
          case _ =>
            report.errorAndAbort(s"All elements of a Layout tuple must be GBuffer[?], found element $idx: ${tpe.show}")
      }

      '{
        new Layout[L] {
          def toBindings(layout: L): Seq[GBuffer[?]] =
            layout.productIterator.map(_.asInstanceOf[GBuffer[?]]).toSeq

          def fromBindings(bindings: Seq[GBuffer[?]]): L =
            ${
              val arity = typeArgs.size
              val tupleClass = Symbol.classSymbol(s"scala.Tuple$arity")
              val constructor = Select(New(TypeIdent(tupleClass)), tupleClass.primaryConstructor)
              val typedConstructor = TypeApply(constructor, typeArgs.map(t => TypeTree.of(using t.asType)))
              val seq = '{ bindings.toIndexedSeq }
              val args = typeArgs.zipWithIndex.map { case (tpe, idx) =>
                val binding = Apply(Select.unique(seq.asTerm, "apply"), List(Expr(idx).asTerm))
                TypeApply(Select.unique(binding, "asInstanceOf"), List(Inferred(tpe)))
              }
              Apply(typedConstructor, args).asExprOf[L]
            }
        }
      }
    else
      // For case classes, use field names
      val fields: List[(String, TypeRepr)] = layoutSymbol.caseFields
        .map(_.tree)
        .collect { case ValDef(name, tpt, _) => (name, tpt.tpe) }

      // Validate that all fields are GBuffer[?]
      fields.foreach { (name, tpe) =>
        tpe match
          case AppliedType(tycon, _) if tycon.typeSymbol.name == "GBuffer" =>
            // OK
          case _ =>
            report.errorAndAbort(s"All fields of a Layout must be GBuffer[?], found: $name: ${tpe.show}")
      }

      def constructLayout(args: List[Term]): Expr[L] =
        val constructor = Select(New(TypeIdent(layoutSymbol)), layoutSymbol.primaryConstructor)
        val readyConstructor = layoutType.typeArgs match
          case Nil => constructor
          case x   => TypeApply(constructor, x.map(t => TypeTree.of(using t.asType)))
        Apply(readyConstructor, args).asExprOf[L]

      '{
        new Layout[L] {
          def toBindings(layout: L): Seq[GBuffer[?]] =
            val result = IndexedSeq.newBuilder[GBuffer[?]]
            result.sizeHint(${ Expr(fields.size) })
            ${
              val l = '{ layout }
              val extracted = fields.map: (name, _) =>
                val field = Select.unique(l.asTerm, name).asExprOf[GBuffer[?]]
                '{ result.addOne($field) }.asTerm
              val (block, last) =
                val r = extracted.reverse
                (r.tail.reverse, r.head)
              Block(block, last).asExprOf[Any]
            }
            result.result()

          def fromBindings(bindings: Seq[GBuffer[?]]): L = ${
            val seq = '{ bindings.toIndexedSeq }
            val args = fields.zipWithIndex.map { case ((_, tpe), idx) =>
              val binding = Apply(Select.unique(seq.asTerm, "apply"), List(Expr(idx).asTerm))
              TypeApply(Select.unique(binding, "asInstanceOf"), List(Inferred(tpe)))
            }
            constructLayout(args)
          }
        }
      }

// =============================================================================
// CYFRA RUNTIME - Executes the computation graph
// =============================================================================

/** Runtime for executing GPU computations. */
trait CyfraRuntime:
  /** Materialize a layout by executing its DAG.
    * Returns a new layout with all buffers having Materialized provenance.
    * Each call triggers a fresh execution - no implicit caching.
    */
  def materialize[L: Layout](layout: L): L

  /** Read actual data from a materialized buffer. */
  def readArray[T](buffer: GBuffer[T]): Array[T]

/** Mock runtime for testing - just prints what would be executed. */
class MockCyfraRuntime extends CyfraRuntime:

  def materialize[L: Layout](layout: L): L =
    val layoutInstance = Layout[L]
    val bindings = layoutInstance.toBindings(layout)
    
    // Build execution plan from all buffers in the layout at once
    val execId = UUID.randomUUID()
    println(s"[RUNTIME] Materializing layout with ${bindings.size} buffer(s)")
    
    // Collect and print execution plan (traverses all buffers, tracks visited)
    val plan = buildExecutionPlan(bindings)
    println(s"[RUNTIME] Execution plan (${plan.size} steps):")
    plan.foreach(step => println(s"  -> $step"))
    println("Materialization complete!")
    
    // Create materialized versions of all buffers
    val materializedBindings = bindings.zipWithIndex.map: (buf, idx) =>
      new GBuffer[Any](buf.name, buf.size, Provenance.Materialized(execId, idx))
    
    layoutInstance.fromBindings(materializedBindings)

  def readArray[T](buffer: GBuffer[T]): Array[T] =
    buffer.provenance match
      case Provenance.Materialized(execId, layoutIndex) =>
        println(s"[RUNTIME] Reading data from: ${buffer.name} (exec: ${execId.toString.take(8)}, index: $layoutIndex)")
        // Mock: return empty array
        Array.empty[Any].asInstanceOf[Array[T]]
      case _ =>
        throw new IllegalStateException(s"Cannot read from non-materialized buffer: ${buffer.name}")

  private def executionName(execution: GExecution[?, ?, ?]): String =
    execution match
      case p: GProgram[?, ?] => p.name
      case _ => execution.getClass.getSimpleName

  /** Build execution plan for all output buffers, tracking visited nodes to avoid duplicates. */
  private def buildExecutionPlan(outputs: Seq[GBuffer[?]]): Seq[String] =
    import scala.collection.mutable
    val visitedBuffers = mutable.Set[GBuffer[?]]()
    val visitedExecutions = mutable.Set[GExecution[?, ?, ?]]()
    val plan = mutable.ArrayBuffer[String]()

    def traverse(buffer: GBuffer[?]): Unit =
      if visitedBuffers.contains(buffer) then return
      visitedBuffers.add(buffer)

      buffer.provenance match
        case Provenance.External(name) =>
          plan += s"Load external: $name"
        case Provenance.ExecutionNode(execution, inputs, outputIndex) =>
          // First traverse all inputs
          inputs.foreach(traverse)
          // Then add this execution (only once per execution)
          if !visitedExecutions.contains(execution) then
            visitedExecutions.add(execution)
            plan += s"Execute: ${executionName(execution)}"
        case Provenance.Copied(source, ri, wi, sz) =>
          traverse(source)
          plan += s"Copy: ${source.name}[$ri:${ri+sz}] -> ${buffer.name}[$wi:${wi+sz}]"
        case Provenance.Materialized(executionId, layoutIndex) =>
          plan += s"Already materialized: ${executionId.toString.take(8)}[$layoutIndex]"

    outputs.foreach(traverse)
    plan.toSeq

// =============================================================================
// GEXECUTION - The composable execution type
// =============================================================================

/** A GPU execution pipeline. PURE - no side effects during composition.
  *
  * @tparam Params Compile-time parameters (sizes, configuration)
  * @tparam In The input type (what you provide to dispatch)
  * @tparam Out The output type (what dispatch returns)
  */
sealed trait GExecution[Params, In, Out]:
  def params: Params

  /** Dispatch with input - PURE, builds DAG by creating buffers with provenance. */
  def dispatch(input: In): Out

  /** Transform the output. */
  def map[Out2](f: Out => Out2): GExecution[Params, In, Out2] =
    val self = this
    new GExecution[Params, In, Out2]:
      def params = self.params
      def dispatch(input: In): Out2 = f(self.dispatch(input))

  /** Transform the input (contravariant). */
  def contramap[In2](f: In2 => In): GExecution[Params, In2, Out] =
    val self = this
    new GExecution[Params, In2, Out]:
      def params = self.params
      def dispatch(input: In2): Out = self.dispatch(f(input))

object GExecution:
  /** Identity execution - just passes through the input. */
  def from[In]: GExecution[Unit, In, In] =
    new GExecution[Unit, In, In]:
      def params = ()
      def dispatch(input: In): In = input

// =============================================================================
// GPROGRAM - A single GPU compute program
// =============================================================================

/** A GPU compute program. PURE dispatch - creates output buffers with provenance.
  */
trait GProgram[Params, L: Layout] extends GExecution[Params, L, L]:
  def name: String
  def workgroupSize: (Int, Int, Int)
  def dispatchSize(layout: L, params: Params): (Int, Int, Int)

  /** Creates output layout structure (with placeholder buffers). */
  def createOutput(input: L): L

  /** Dispatch: creates output layout with buffers that have ExecutionNode provenance.
    * The provenance references this GExecution (which is also a GProgram).
    */
  def dispatch(input: L): L =
    val layout = Layout[L]
    val inputBuffers = layout.toBindings(input)
    val outputTemplate = createOutput(input)

    // Get all output buffers and create versions with ExecutionNode provenance
    val outputBuffers = layout.toBindings(outputTemplate)
    val execution: GExecution[Params, L, L] = this
    val updatedBuffers = outputBuffers.zipWithIndex.map: (buf, outputIndex) =>
      new GBuffer[Any](buf.name, buf.size, Provenance.ExecutionNode(execution, inputBuffers, outputIndex))

    // Reconstruct the layout using fromBindings (macro-generated)
    layout.fromBindings(updatedBuffers)

object GProgram:
  def apply[Params, L: Layout](
    programName: String,
    programParams: Params,
    workgroup: (Int, Int, Int) = (256, 1, 1),
    dispatchFn: (L, Params) => (Int, Int, Int) = (_: L, _: Params) => (1, 1, 1),
  )(outputFactory: L => L): GProgram[Params, L] =
    new GProgram[Params, L]:
      val name = programName
      val params = programParams
      val workgroupSize = workgroup
      def dispatchSize(layout: L, p: Params) = dispatchFn(layout, p)
      def createOutput(input: L) = outputFactory(input)
