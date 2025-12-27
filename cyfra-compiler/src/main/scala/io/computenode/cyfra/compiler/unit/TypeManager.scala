package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.core.expression.Value
import izumi.reflect.Tag

import scala.collection.mutable

class TypeManager(block: List[IR[?]] = Nil, cache: Map[Tag[?], RefIR[Unit]] = Map.empty):
  def getType(value: Value[?]): (RefIR[Unit], TypeManager) =
    cache.get(value.tag) match
      case Some(value) => (value, this)
      case None        => ???

//  private def computeType(tag: Tag[?]): IR[Unit] =
//    ???

  def output: List[IR[?]] = block.reverse
