package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.GBuffer
import Result.Result

import scala.collection.mutable.Map as MMap

case class SimContext(exprMap: MMap[Int, Result] = MMap(), bufMap: MMap[GBuffer[?], Array[Result]] = MMap(), var reads: List[Read] = Nil):
  def addBuffer(buffer: GBuffer[?], array: Array[Result]): Unit = bufMap.addOne(buffer -> array)
  def addResult(treeid: Int, result: Result): Unit = exprMap.addOne(treeid -> result)
  def addRead(buffer: GBuffer[?], index: Int): Unit = reads ::= Read(buffer, index)
  def read(buffer: GBuffer[?], index: Int): Result = bufMap(buffer)(index)
  def lookup(treeid: Int): Result = exprMap(treeid)

case class Read(buffer: GBuffer[?], index: Int)
