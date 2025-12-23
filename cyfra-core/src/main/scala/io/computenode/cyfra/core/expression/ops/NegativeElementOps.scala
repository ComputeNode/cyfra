package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

import scala.annotation.targetName

given [T <: NegativeType: Value]: NegativeElementOps[T] with {}
given [T <: NegativeType: Value]: NegativeElementOps[Vec2[T]] with {}
given [T <: NegativeType: Value]: NegativeElementOps[Vec3[T]] with {}
given [T <: NegativeType: Value]: NegativeElementOps[Vec4[T]] with {}

trait NegativeElementOps[T]

extension [T: {NegativeElementOps, Value}](self: T)
  @targetName("neg")
  def unary_- : T = self.map(BuildInFunction.Neg)
  @targetName("rem")
  infix def rem(that: T): T = self.map(that)(BuildInFunction.Rem)
