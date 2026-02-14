package io.computenode.cyfra.core

import io.computenode.cyfra.core.GExecution.*
import io.computenode.cyfra.core.layout.*

/** A GPU execution pipeline. PURE - no side effects during composition.
  * Params are passed at execution/dispatch time, NOT stored.
  *
  * @tparam Params Compile-time parameters (sizes, configuration)
  * @tparam In The input layout type
  * @tparam Out The output layout type
  */
trait GExecution[Params, In, Out]:
  /** Transform the output. */
  def map[Out2](f: Out => Out2): GExecution[Params, In, Out2] =
    Mapped(this, f)

  /** Transform the input (contravariant). */
  def contramap[In2](f: In2 => In): GExecution[Params, In2, Out] =
    Contramapped(this, f)

  /** Transform params. */
  def contramapParams[P2](f: P2 => Params): GExecution[P2, In, Out] =
    ParamsMapped(this, f)

object GExecution:
  /** Identity execution - just passes through the input. */
  def identity[In]: GExecution[Unit, In, In] =
    Identity()

  /** Create an execution from a pure function. */
  def pure[In, Out](f: In => Out): GExecution[Unit, In, Out] =
    Identity[In]().map(f)

  // === Case classes for execution structure ===

  private[core] case class Identity[L]() extends GExecution[Unit, L, L]

  private[core] case class Mapped[P, I, O, O2](
    underlying: GExecution[P, I, O],
    f: O => O2,
  ) extends GExecution[P, I, O2]

  private[core] case class Contramapped[P, I, I2, O](
    underlying: GExecution[P, I, O],
    f: I2 => I,
  ) extends GExecution[P, I2, O]

  private[core] case class ParamsMapped[P, P2, I, O](
    underlying: GExecution[P, I, O],
    f: P2 => P,
  ) extends GExecution[P2, I, O]
