package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class SizedArray[T: Value, N <: Int: ValueOf]:
  def length: Int = valueOf[N]

object SizedArray:
  final class SizedArrayImpl[T: Value, N <: Int: ValueOf: Tag](val block: ExpressionBlock[SizedArray[T, N]])
      extends SizedArray[T, N]
      with ExpressionHolder[SizedArray[T, N]]

  given [T: Value, N <: Int: ValueOf: Tag]: Value[SizedArray[T, N]] with
    protected def extractUnsafe(ir: ExpressionBlock[SizedArray[T, N]]): SizedArray[T, N] = new SizedArrayImpl(ir)
    given Tag[T] = Value[T].tag
    override def tag: Tag[SizedArray[T, N]] = Tag[SizedArray[T, N]]
    override def composites: List[Value[?]] = List(Value[T])
    override def baseTag: Option[Tag[?]] = Some(Tag[SizedArray].asInstanceOf[Tag[?]])
    override val arrayLength: Option[Int] =
      require(valueOf[N] > 0, "Sized-array length must be positive")
      Some(valueOf[N])
