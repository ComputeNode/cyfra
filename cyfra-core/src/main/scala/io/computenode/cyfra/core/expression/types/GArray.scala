package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class GArray[T: Value]
object GArray:
  final class GArrayImpl[T: Value](val block: ExpressionBlock[GArray[T]]) extends GArray[T] with ExpressionHolder[GArray[T]]

  given [T: Value]: Value[GArray[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[GArray[T]]): GArray[T] = throw UnsupportedOperationException(
      "Cant have direct access to runtime array",
    )
    given Tag[T] = Value[T].tag
    def tag: Tag[GArray[T]] = Tag[GArray[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[GArray].asInstanceOf[Tag[?]])
