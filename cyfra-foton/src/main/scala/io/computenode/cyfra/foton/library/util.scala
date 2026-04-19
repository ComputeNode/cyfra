package io.computenode.cyfra.foton.library

import io.computenode.cyfra.core.expression.CustomFunction
import io.computenode.cyfra.core.expression.CustomFunction.CustomFunction1
import io.computenode.cyfra.core.expression.ops.Functions
import io.computenode.cyfra.core.expression.ops.Functions.cross
import io.computenode.cyfra.core.memory.Variable
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.direct.GIO

type fRGBA = (Float, Float, Float, Float)

case class Epsilon(eps: Float)

given Epsilon = Epsilon(0.00001f)

extension (f32: Float32)
  inline def =~=(other: Float32)(using epsilon: Epsilon): Bool =
    abs(f32 - other) < epsilon.eps


def scalarTriple(u: Vec3[Float32], v: Vec3[Float32], w: Vec3[Float32]): Float32 = (u cross v) dot w




