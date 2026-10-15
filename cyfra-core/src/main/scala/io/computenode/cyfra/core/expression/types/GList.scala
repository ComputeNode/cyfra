package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

/** A fixed-size GPU array whose length is part of its type. */
abstract class GList[T: Value, N <: Int: ValueOf]:
  def length: Int = valueOf[N]

object GList:
  final class GListImpl[T: Value, N <: Int: ValueOf: Tag](val block: ExpressionBlock[GList[T, N]])
      extends GList[T, N]
      with ExpressionHolder[GList[T, N]]

  given [T: Value, N <: Int: ValueOf: Tag]: Value[GList[T, N]] with
    protected def extractUnsafe(ir: ExpressionBlock[GList[T, N]]): GList[T, N] = new GListImpl(ir)
    given Tag[T] = Value[T].tag
    override def tag: Tag[GList[T, N]] = Tag[GList[T, N]]
    override def composites: List[Value[?]] = List(Value[T])
    override def baseTag: Option[Tag[?]] = Some(Tag[GList].asInstanceOf[Tag[?]])
    override val arrayLength: Option[Int] =
      require(valueOf[N] > 0, "Fixed-list length must be positive")
      Some(valueOf[N])
