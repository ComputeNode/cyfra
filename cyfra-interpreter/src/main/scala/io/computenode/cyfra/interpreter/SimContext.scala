package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.GBuffer
import Result.Result

case class SimContext(
  exprMap: Map[Int, Result] = Map(),
  bufMap: Map[GBuffer[?], Array[Result]] = Map(),
  reads: List[Read] = Nil,
  writes: List[Write] = Nil,
):
  def addBuffer(buffer: GBuffer[?], array: Array[Result]): SimContext = copy(bufMap = bufMap + (buffer -> array))
  def addResult(treeid: Int, result: Result): SimContext = copy(exprMap = exprMap + (treeid -> result))
  def addRead(buffer: GBuffer[?], index: Int): SimContext = copy(reads = Read(buffer, index) :: reads)
  def lookupRead(buffer: GBuffer[?], index: Int): Result = bufMap(buffer)(index)
  def lookupExpr(treeid: Int): Result = exprMap(treeid)

case class Read(buffer: GBuffer[?], index: Int)
