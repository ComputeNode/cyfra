package io.computenode.cyfra.core.expression

given Conversion[Int, Int32] with
  def apply(value: Int): Int32 = Int32(value)

given Conversion[Float, Float32] with
  def apply(value: Float): Float32 = Float32(value)
