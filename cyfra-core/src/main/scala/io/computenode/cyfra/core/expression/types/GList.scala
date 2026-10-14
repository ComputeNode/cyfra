package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

/** A fixed-size GPU array whose length is part of its type. */
abstract class GList[T: Value, N <: Int & Singleton]

object GList:
  final class ListValue[T: Value, N <: Int & Singleton: ValueOf: Tag] extends Value[GList[T, N]]:
    val length: Int = valueOf[N]
    require(length > 0, "Fixed-list length must be positive")
    given Tag[T] = Value[T].tag
    def tag: Tag[GList[T, N]] = Tag[GList[T, N]]
    def baseTag: Option[Tag[?]] = Some(Tag[GList].asInstanceOf[Tag[?]])
    def composites: List[Value[?]] = List(Value[T])
    protected def extractUnsafe(ir: ExpressionBlock[GList[T, N]]): GList[T, N] =
      given Value[GList[T, N]] = this
      new GList[T, N] with ExpressionHolder[GList[T, N]]:
        val block: ExpressionBlock[GList[T, N]] = ir

  given [T: Value, N <: Int & Singleton: ValueOf: Tag]: ListValue[T, N] = new ListValue[T, N]
