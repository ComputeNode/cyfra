package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.{GBuffer, GUniform}
import Result.Result

enum Reads:
  case ReadBuf(buffer: GBuffer[?], index: Int)
  case ReadUni(uniform: GUniform[?])
export Reads.*

enum Writes:
  case WriteBuf(buffer: GBuffer[?], index: Int, value: Result)
  case WriteUni(uni: GUniform[?], value: Result)
export Writes.*

case class SimContext(
  bufMap: Map[GBuffer[?], Array[Result]] = Map(),
  values: List[Result] = Nil,
  writes: List[Writes] = Nil,
  reads: List[Reads] = Nil,
):
  def addBuffer(buffer: GBuffer[?], array: Array[Result]): SimContext = copy(bufMap = bufMap + (buffer -> array))

  def addRead(read: Reads): SimContext = read match
    case ReadBuf(buffer, index) => copy(reads = ReadBuf(buffer, index) :: reads)
    case ReadUni(uniform)       => ???

  def addWrite(write: Writes): SimContext = write match
    case WriteBuf(buffer, index, value) =>
      val newArray = bufMap(buffer).updated(index, value)
      val newWrites = WriteBuf(buffer, index, value) :: writes
      copy(bufMap = bufMap.updated(buffer, newArray), writes = newWrites)
    case WriteUni(uni, value) => ???

  def addResult(res: Result) = copy(values = res :: values)
  def lookup(buffer: GBuffer[?], index: Int): Result = bufMap(buffer)(index)
