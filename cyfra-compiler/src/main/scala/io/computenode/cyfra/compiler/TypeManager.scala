package io.computenode.cyfra.compiler

import io.computenode.cyfra.compiler.ir.{IR, IRs}

import scala.collection.mutable
import izumi.reflect.Tag

class TypeManager:
  private val block: List[IR[?]] = Nil
  private val compiled: mutable.Map[Tag[?], IR[Unit]] = mutable.Map()

  def getType(tag: Tag[?]): IR[Unit] =
    compiled.getOrElseUpdate(tag, ???)

  private def computeType(tag: Tag[?]): IR[Unit] =
    ???
