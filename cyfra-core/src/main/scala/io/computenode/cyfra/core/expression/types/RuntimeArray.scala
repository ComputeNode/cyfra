package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class RuntimeArray[T: Value]
object RuntimeArray:
  given [T: Value]: Value[RuntimeArray[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[RuntimeArray[T]]): RuntimeArray[T] = throw UnsupportedOperationException(
      "Can't have direct access to runtime array",
    )
    given Tag[T] = Value[T].tag
    def tag: Tag[RuntimeArray[T]] = Tag[RuntimeArray[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[RuntimeArray].asInstanceOf[Tag[?]])
