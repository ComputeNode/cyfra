package io.computenode.cyfra.core.expression

trait ExpressionHolder[A: Value]:
  def block: ExpressionBlock[A]
