package io.computenode.cyfra.foton.library

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.{GArray, Int32}
import io.computenode.cyfra.core.memory.GBuffer
import io.computenode.cyfra.dsl.direct.GIO

class GArray2D[T: Value](width: Int, val arr: GBuffer[GArray[T]]):
  def at(x: Int32, y: Int32): T =
    val exp = GIO.reify:
      val idx = y * width + x
      val focus = arr.focus(_.at(idx))
      GIO.read(focus)
    Value[T].extract(exp)
