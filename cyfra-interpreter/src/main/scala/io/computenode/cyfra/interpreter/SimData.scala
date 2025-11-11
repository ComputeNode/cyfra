package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.{GBuffer, GUniform}

case class SimData(bufMap: Map[GBuffer[?], Array[Result]] = Map(), uniMap: Map[GUniform[?], Result] = Map()):
  def addBuffer(buffer: GBuffer[?], array: Array[Result]) = copy(bufMap = bufMap + (buffer -> array))
  def addUniform(uniform: GUniform[?], value: Result) = copy(uniMap = uniMap + (uniform -> value))

  def lookup(buffer: GBuffer[?], index: Int): Result = bufMap(buffer)(index)
  def lookupUni(uniform: GUniform[?]): Result = uniMap(uniform)

  def write(write: Write): SimData = write match
    case WriteBuf(buffer, index, value) =>
      val newArray = bufMap(buffer).updated(index, value)
      copy(bufMap = bufMap.updated(buffer, newArray))
    case WriteUni(uni, value) => copy(uniMap = uniMap.updated(uni, value))

  def writeToBuffer(buffer: GBuffer[?], indices: Results, writeValues: Results): SimData =
    val array = bufMap(buffer)
    val newArray = array.clone()
    for (invocId, writeIndex) <- indices do newArray(writeIndex.asInstanceOf[Int]) = writeValues(invocId)
    val newBufMap = bufMap.updated(buffer, newArray)
    copy(bufMap = newBufMap)
