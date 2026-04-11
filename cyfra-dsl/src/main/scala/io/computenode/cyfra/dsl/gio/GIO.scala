package io.computenode.cyfra.dsl.gio

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Expression.{CustomTreeId, PhantomExpression, treeidState, *, given}
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32, UInt32, Float16, Float32, Vec3, Vec4}
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr
import io.computenode.cyfra.dsl.binding.{GBuffer, ReadBuffer, WriteBuffer}
import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.gio.GIO.*
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.dsl.control.When
import izumi.reflect.Tag

/**
 * GPU I/O monad for representing side-effectful GPU operations.
 * Supports buffer reads/writes, synchronization barriers, and workgroup-level operations.
 */
trait GIO[T <: Value]:

  def flatMap[U <: Value](f: T => GIO[U]): GIO[U] = FlatMap(this, f(this.underlying))

  def map[U <: Value](f: T => U): GIO[U] = flatMap(t => GIO.pure(f(t)))

  private[cyfra] def underlying: T

object GIO:

  case class Pure[T <: Value](value: T) extends GIO[T]:
    override def underlying: T = value

  case class FlatMap[T <: Value, U <: Value](gio: GIO[T], next: GIO[U]) extends GIO[U]:
    override def underlying: U = next.underlying

  /** Loop that repeats n times without accumulator.
    *
    * @param n Number of iterations
    * @param f Body GIO to execute
    * @param unroll Whether to hint the GPU compiler to unroll this loop
    */
  case class Repeat(n: Int32, f: GIO[?], unroll: Boolean = false) extends GIO[Empty]:
    override def underlying: Empty = Empty()

  /** Folding repeat with accumulator - enables accumulation across iterations with barriers.
    *
    * @param n Number of iterations
    * @param init Initial accumulator value
    * @param body Body GIO that returns new accumulator
    * @param accTreeId Treeid of the CurrentFoldRepeatAcc phantom for binding
    * @param unroll Whether to hint the GPU compiler to unroll this loop
    */
  case class FoldRepeat[A <: Value](n: Int32, init: A, body: GIO[A], accTreeId: Int, unroll: Boolean = false) extends GIO[A]:
    override def underlying: A = body.underlying

  case class Printf(format: String, args: Value*) extends GIO[Empty]:
    override def underlying: Empty = Empty()

  /** Conditional execution - executes body only if condition is true.
    * Compiled to proper if-then structure (OpSelectionMerge + OpBranchConditional).
    */
  case class ConditionalWhen(cond: GBoolean, body: GIO[?]) extends GIO[Empty]:
    override def underlying: Empty = Empty()

  /** Memory and execution barrier for workgroup synchronization. */
  case object WorkgroupBarrier extends GIO[Empty]:
    override def underlying: Empty = Empty()

  def pure[T <: Value](value: T): GIO[T] = Pure(value)

  def value[T <: Value](value: T): GIO[T] = Pure(value)

  case object CurrentRepeatIndex extends PhantomExpression[Int32] with CustomTreeId:
    override val treeid: Int = treeidState.getAndIncrement()

  /** Phantom expression for the current accumulator value in foldRepeat. */
  case class CurrentFoldRepeatAcc[A <: Value: Tag](init: A, tid: Int) extends PhantomExpression[A] with CustomTreeId:
    override val treeid: Int = tid

  def repeat(n: Int32)(f: Int32 => GIO[?]): GIO[Empty] =
    Repeat(n, f(fromExpr(CurrentRepeatIndex)), unroll = false)

  /** Repeat with loop unroll hint. The GPU compiler will attempt to fully unroll
    * this loop for better performance. Use for small, fixed-size loops.
    */
  def repeatUnroll(n: Int32)(f: Int32 => GIO[?]): GIO[Empty] =
    Repeat(n, f(fromExpr(CurrentRepeatIndex)), unroll = true)

  /** Folding repeat - accumulates a value across iterations, supporting barriers.
    *
    * Unlike `GSeq.fold`, this supports side effects (barriers, writes) within the loop body.
    * The body receives the current iteration index and current accumulator value,
    * and returns the new accumulator value wrapped in GIO.
    *
    * @param n Number of iterations
    * @param init Initial accumulator value
    * @param body Function taking (iterationIndex, currentAcc) and returning new acc in GIO
    * @return Final accumulated value
    */
  def foldRepeat[A <: Value: {FromExpr, Tag}](n: Int32, init: A)(body: (Int32, A) => GIO[A]): GIO[A] =
    val tid = treeidState.getAndIncrement()
    val accExpr = CurrentFoldRepeatAcc(init, tid)
    FoldRepeat(n, init, body(fromExpr(CurrentRepeatIndex), fromExpr(accExpr)), tid, unroll = false)

  /** Folding repeat with loop unroll hint. The GPU compiler will attempt to fully
    * unroll this loop for better performance. Use for small, fixed-size inner loops
    * (e.g., head dimension in attention, vector dot products).
    *
    * @param n Number of iterations (should be a small constant for effective unrolling)
    * @param init Initial accumulator value
    * @param body Function taking (iterationIndex, currentAcc) and returning new acc in GIO
    * @return Final accumulated value
    */
  def foldRepeatUnroll[A <: Value: {FromExpr, Tag}](n: Int32, init: A)(body: (Int32, A) => GIO[A]): GIO[A] =
    val tid = treeidState.getAndIncrement()
    val accExpr = CurrentFoldRepeatAcc(init, tid)
    FoldRepeat(n, init, body(fromExpr(CurrentRepeatIndex), fromExpr(accExpr)), tid, unroll = true)

  def write[T <: Value](buffer: GBuffer[T], index: Int32, value: T): GIO[Empty] =
    WriteBuffer(buffer, index, value)

  def printf(format: String, args: Value*): GIO[Empty] =
    Printf(s"|$format", args*)

  def when(cond: GBoolean)(thenCode: GIO[?]): GIO[Empty] =
    ConditionalWhen(cond, thenCode)

  def read[T <: Value: {FromExpr, Tag}](buffer: GBuffer[T], index: Int32): T =
    fromExpr(ReadBuffer(buffer, index))

  import scala.annotation.targetName

  // ─────────────────────────────────────────────────────────────────────────────
  // Global Invocation
  // ─────────────────────────────────────────────────────────────────────────────

  /** Global invocation index (gl_GlobalInvocationID.x). */
  def invocationId: Int32 =
    fromExpr(InvocationId)

  // ─────────────────────────────────────────────────────────────────────────────
  // Workgroup Primitives
  // ─────────────────────────────────────────────────────────────────────────────

  /** Local invocation index within workgroup (gl_LocalInvocationIndex). */
  def localInvocationIndex: Int32 =
    fromExpr(LocalInvocationIndex)

  /** Local invocation ID as 3D vector (gl_LocalInvocationID). */
  def localInvocationId: Vec3[Int32] =
    fromExpr(LocalInvocationId)

  /** Workgroup ID as 3D vector (gl_WorkGroupID). */
  def workgroupId: Vec3[Int32] =
    fromExpr(WorkgroupId)

  /** Number of workgroups as 3D vector (gl_NumWorkGroups). */
  def numWorkgroups: Vec3[Int32] =
    fromExpr(NumWorkgroups)

  /** Synchronization barrier for workgroup memory and execution. */
  def barrier: GIO[Empty] = WorkgroupBarrier

  // ─────────────────────────────────────────────────────────────────────────────
  // Subgroup Primitives
  // ─────────────────────────────────────────────────────────────────────────────

  /** Subgroup ID within the workgroup. */
  def subgroupId: Int32 =
    fromExpr(SubgroupId)

  /** Local invocation ID within the subgroup. */
  def subgroupLocalInvocationId: Int32 =
    fromExpr(SubgroupLocalInvocationId)

  /** Size of subgroup (typically 32 for NVIDIA, 64 for AMD). */
  def subgroupSize: Int32 =
    fromExpr(SubgroupSize)

  // ─────────────────────────────────────────────────────────────────────────────
  // Subgroup Collective Operations
  // ─────────────────────────────────────────────────────────────────────────────

  /** Reduces values across the subgroup using addition. */
  def subgroupAdd(value: Int32): Int32 =
    fromExpr(SubgroupAddI(value, SubgroupOp.Reduce))

  /** Reduces values across the subgroup using addition. */
  @targetName("subgroupAddF16")
  def subgroupAdd(value: Float16): Float16 =
    fromExpr(SubgroupAddF16(value, SubgroupOp.Reduce))

  /** Reduces values across the subgroup using addition. */
  def subgroupAdd(value: Float32): Float32 =
    fromExpr(SubgroupAddF(value, SubgroupOp.Reduce))

  /** Inclusive prefix sum across the subgroup. */
  def subgroupInclusiveAdd(value: Int32): Int32 =
    fromExpr(SubgroupAddI(value, SubgroupOp.InclusiveScan))

  /** Inclusive prefix sum across the subgroup. */
  @targetName("subgroupInclusiveAddF16")
  def subgroupInclusiveAdd(value: Float16): Float16 =
    fromExpr(SubgroupAddF16(value, SubgroupOp.InclusiveScan))

  /** Inclusive prefix sum across the subgroup. */
  def subgroupInclusiveAdd(value: Float32): Float32 =
    fromExpr(SubgroupAddF(value, SubgroupOp.InclusiveScan))

  /** Exclusive prefix sum across the subgroup. */
  def subgroupExclusiveAdd(value: Int32): Int32 =
    fromExpr(SubgroupAddI(value, SubgroupOp.ExclusiveScan))

  /** Exclusive prefix sum across the subgroup. */
  @targetName("subgroupExclusiveAddF16")
  def subgroupExclusiveAdd(value: Float16): Float16 =
    fromExpr(SubgroupAddF16(value, SubgroupOp.ExclusiveScan))

  /** Exclusive prefix sum across the subgroup. */
  def subgroupExclusiveAdd(value: Float32): Float32 =
    fromExpr(SubgroupAddF(value, SubgroupOp.ExclusiveScan))

  /** Reduces values across the subgroup using minimum. */
  def subgroupMin(value: Int32): Int32 =
    fromExpr(SubgroupMinI(value, SubgroupOp.Reduce))

  /** Reduces values across the subgroup using minimum. */
  @targetName("subgroupMinF16")
  def subgroupMin(value: Float16): Float16 =
    fromExpr(SubgroupMinF16(value, SubgroupOp.Reduce))

  /** Reduces values across the subgroup using minimum. */
  def subgroupMin(value: Float32): Float32 =
    fromExpr(SubgroupMinF(value, SubgroupOp.Reduce))

  /** Reduces values across the subgroup using maximum. */
  def subgroupMax(value: Int32): Int32 =
    fromExpr(SubgroupMaxI(value, SubgroupOp.Reduce))

  /** Reduces values across the subgroup using maximum. */
  @targetName("subgroupMaxF16")
  def subgroupMax(value: Float16): Float16 =
    fromExpr(SubgroupMaxF16(value, SubgroupOp.Reduce))

  /** Reduces values across the subgroup using maximum. */
  def subgroupMax(value: Float32): Float32 =
    fromExpr(SubgroupMaxF(value, SubgroupOp.Reduce))

  /** Broadcasts a value from a specific lane to all lanes in the subgroup. */
  def subgroupBroadcast[T <: Value.Scalar: {FromExpr, Tag}](value: T, lane: Int32): T =
    fromExpr(SubgroupBroadcast(value, lane))

  /** Broadcasts a value from the first active lane to all lanes in the subgroup. */
  def subgroupBroadcastFirst[T <: Value.Scalar: {FromExpr, Tag}](value: T): T =
    fromExpr(SubgroupBroadcastFirst(value))

  /** Shuffles a value from another lane in the subgroup. */
  def subgroupShuffle[T <: Value.Scalar: {FromExpr, Tag}](value: T, lane: Int32): T =
    fromExpr(SubgroupShuffle(value, lane))

  /** Shuffles a value using XOR of lane index with mask.
    * This is useful for butterfly/tree reductions where each thread exchanges
    * data with thread at (laneId XOR mask). For example:
    *   - mask=1: lanes 0↔1, 2↔3, 4↔5, ...
    *   - mask=2: lanes 0↔2, 1↔3, 4↔6, ...
    *   - mask=4: lanes 0↔4, 1↔5, 2↔6, ...
    */
  def subgroupShuffleXor[T <: Value.Scalar: {FromExpr, Tag}](value: T, mask: Int32): T =
    fromExpr(SubgroupShuffleXor(value, mask))