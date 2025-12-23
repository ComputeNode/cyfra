package io.computenode.cyfra.core

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.*
import io.computenode.cyfra.core.expression.ops.given
import io.computenode.cyfra.core.expression.given

@main
def main(): Unit =
  val x: Mat4x4[Float32] = Mat4x4(1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f)
  val y: Vec4[Float32] = Vec4(1.0f, 2.0f, 3.0f, 4.0f)
  val c = x * y
  println("Hello, Cyfra!")
  println(summon[Value[Mat4x4[Float32]]].tag)
  println(c)
