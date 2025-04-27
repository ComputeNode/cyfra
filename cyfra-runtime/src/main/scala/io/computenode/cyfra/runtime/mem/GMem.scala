package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.{GStructConstructor, GStructSchema, Value}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride

import java.nio.ByteBuffer

trait GMem[H <: Value]:
  def size: Int
  val data: ByteBuffer

object GMem:
  type fRGBA = (Float, Float, Float, Float)

  def totalStride(gs: GStructSchema[_]): Int = gs.fields.map {
    case (_, fromExpr, t) if t <:< gs.gStructTag =>
      val constructor = fromExpr.asInstanceOf[GStructConstructor[_]]
      totalStride(constructor.schema)
    case (_, _, t) =>
      typeStride(t)
  }.sum