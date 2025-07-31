package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.{GBuffer, GUniform}

case class SimContext(bufMap: Map[GBuffer[?], Array[Result]] = Map(), uniMap: Map[GUniform[?], Result] = Map()):
  def addBuffer(buffer: GBuffer[?], array: Array[Result]) = copy(bufMap = bufMap + (buffer -> array))
  def addUniform(uniform: GUniform[?], value: Result) = copy(uniMap = uniMap + (uniform -> value))

  def lookup(buffer: GBuffer[?], index: Int): Result = bufMap(buffer)(index)
  def lookupUni(uniform: GUniform[?]): Result = uniMap(uniform)

  def addWrite(write: Write): SimContext = write match
    case WriteBuf(_, buffer, index, value) =>
      val newArray = bufMap(buffer).updated(index, value)
      copy(bufMap = bufMap.updated(buffer, newArray))
    case WriteUni(_, uni, value) => copy(uniMap = uniMap.updated(uni, value))
