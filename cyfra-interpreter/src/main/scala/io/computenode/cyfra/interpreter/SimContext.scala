package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.GBuffer
import Result.Result

case class SimContext(bufMap: Map[GBuffer[?], Array[Result]] = Map(), reads: List[Read] = Nil, writes: List[Write] = Nil):
  def addBuffer(buffer: GBuffer[?], array: Array[Result]): SimContext = copy(bufMap = bufMap + (buffer -> array))
  def addRead(buffer: GBuffer[?], index: Int): SimContext = copy(reads = Read(buffer, index) :: reads)
  def addWrite(buffer: GBuffer[?], index: Int, value: Result): SimContext =
    val newArray = bufMap(buffer).updated(index, value)
    val newWrites = Write(buffer, index, value) :: writes
    copy(bufMap = bufMap.updated(buffer, newArray), writes = newWrites)
  def lookup(buffer: GBuffer[?], index: Int): Result = bufMap(buffer)(index)

case class Read(buffer: GBuffer[?], index: Int)
case class Write(buffer: GBuffer[?], index: Int, value: Result)
