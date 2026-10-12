package io.computenode.cyfra.foton.library

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.{GArray, Int32}
import io.computenode.cyfra.core.memory.GBuffer
import io.computenode.cyfra.dsl.direct.GIO

/** Reads a buffer element from pure code. The read is emitted where the value is first used. */
extension [T: Value](arr: GBuffer[GArray[T]])
  def at(index: Int32): T =
    Value[T].extractUnchecked(GIO.reify(GIO.read(arr.focus(_.at(index)))))

class GArray2D[T: Value](width: Int, val arr: GBuffer[GArray[T]]):
  def at(x: Int32, y: Int32): T = arr.at(y * width + x)
