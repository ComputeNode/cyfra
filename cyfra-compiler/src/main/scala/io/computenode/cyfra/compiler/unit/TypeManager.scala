package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.core.expression.Value
import izumi.reflect.Tag

import scala.collection.mutable

class TypeManager extends Manager:
  private val block: List[IR[?]] = Nil
  private val compiled: mutable.Map[Tag[?], RefIR[Unit]] = mutable.Map()

  def getType(value: Value[?]): RefIR[Unit] =
    compiled.getOrElseUpdate(value.tag, ???)

//  private def computeType(tag: Tag[?]): IR[Unit] =
//    ???

  def output: List[IR[?]] = block.reverse
