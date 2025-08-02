package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import izumi.reflect.Tag

object Interpreter:
  private def interpretPure(gio: Pure[?], sc: SimContext): SimContext = gio match
    // TODO needs fixing, throws ClassCastException, Pure[T] should be Pure[T <: Value]
    case Pure(value) => Simulate.sim(value.asInstanceOf[Value], sc) // no writes here

  private def interpretWriteBuffer(gio: WriteBuffer[?], sc: SimContext): SimContext = gio match
    case WriteBuffer(buffer, index, value) =>
      val sc1 = Simulate.sim(index, sc) // get the write index for each invocation
      val SimContext(writeVals, records, data) = Simulate.sim(value, sc1) // get the values to be written

      // write the values to the buffer, update records with writes
      val indices = sc1.results
      val newData = data.writeToBuffer(buffer, indices, writeVals)
      val writes = indices.map: (invocId, ind) =>
        invocId -> WriteBuf(buffer, ind.asInstanceOf[Int], writeVals(invocId))
      val newRecords = records.addWrites(writes)

      SimContext(writeVals, newRecords, newData)

  private def interpretWriteUniform(gio: WriteUniform[?], sc: SimContext): SimContext = gio match
    case WriteUniform(uniform, value) =>
      // get the uniform value to be written (same for all invocations)
      val SimContext(writeVals, records, data) = Simulate.sim(value, sc)

      // write the (single) value to the uniform, update records with writes
      val uniVal = writeVals.values.head
      val writes = writeVals.map((invocId, res) => invocId -> WriteUni(uniform, res))
      val newData = data.write(WriteUni(uniform, uniVal))
      val newRecords = records.addWrites(writes)

      SimContext(writeVals, newRecords, newData)

  private def interpretOne(gio: GIO[?], sc: SimContext): SimContext = gio match
    case p: Pure[?]          => interpretPure(p, sc)
    case wb: WriteBuffer[?]  => interpretWriteBuffer(wb, sc)
    case wu: WriteUniform[?] => interpretWriteUniform(wu, sc)
    case _                   => throw IllegalArgumentException("interpretOne: invalid GIO")

  @annotation.tailrec
  private def interpretMany(gios: List[GIO[?]], sc: SimContext): SimContext = gios match
    case FlatMap(gio, next) :: tail => interpretMany(gio :: next :: tail, sc)
    case Repeat(n, f) :: tail       =>
      // does the value of n vary by invocation?
      // can different invocations run different numbers of GIOs?
      val newSc = Simulate.sim(n, sc)
      val repeat = newSc.results.values.head.asInstanceOf[Int]
      val newGios = (0 until repeat).map(i => f(i)).toList
      interpretMany(newGios ::: tail, newSc)
    case head :: tail => interpretMany(tail, interpretOne(head, sc))
    case Nil          => sc

  def interpret(gio: GIO[?], sc: SimContext): SimContext = interpretMany(List(gio), sc)
