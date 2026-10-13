package io.computenode.cyfra.core.expression

abstract class OperatorImpure:
  def name: String = this.getClass.getSimpleName.replace("$", "")
  override def toString: String = s"operatorImpure $name"

object OperatorImpure:
  abstract class OperatorImpure0 extends OperatorImpure
  abstract class OperatorImpure1 extends OperatorImpure
  abstract class OperatorImpure2 extends OperatorImpure
  abstract class OperatorImpure3 extends OperatorImpure
  abstract class OperatorImpure4 extends OperatorImpure
