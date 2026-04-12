package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock, Value}
import izumi.reflect.Tag

val unitZero = Expression.Constant[Unit](())
given Value.Scalar[Unit] with
  protected def extractUnsafe(ir: ExpressionBlock[Unit]): Unit = ()
  def tag: Tag[Unit] = Tag[Unit]

given Value.Scalar[Any] with
  protected def extractUnsafe(ir: ExpressionBlock[Any]): Any = ir.result.asInstanceOf[Expression.Constant[Any]].value
  def tag: Tag[Any] = Tag[Any]
