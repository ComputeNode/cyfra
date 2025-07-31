package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import izumi.reflect.Tag

object Interpreter:
  private def interpretPure(gio: Pure[?], sc: SimContext): SimContext = gio match
    case Pure(value) =>
      val SimRes(results, records, newSc) = Simulate.sim(value.asInstanceOf[Value]) // TODO needs fixing
      // newSc.addResult(result)
      ???

  private def interpretWriteBuffer(gio: WriteBuffer[?], sc: SimContext): SimContext = gio match
    case WriteBuffer(buffer, index, value) =>
      val SimRes(n, _, _) = Simulate.sim(index) // Int32, no reads/writes here, don't need resulting context
      val i = n.asInstanceOf[Int]
      val SimRes(res, _, newSc) = Simulate.sim(value)
      // newSc.addWrite(WriteBuf(buffer, i, res))
      ???

  private def interpretWriteUniform(gio: WriteUniform[?], sc: SimContext): SimContext = gio match
    case WriteUniform(uniform, value) =>
      val SimRes(result, _, newSc) = Simulate.sim(value)
      // newSc.addWrite(WriteUni(uniform, result))
      ???

  private def interpretOne(gio: GIO[?], sc: SimContext): SimContext = gio match
    case p: Pure[?]          => interpretPure(p, sc)
    case wb: WriteBuffer[?]  => interpretWriteBuffer(wb, sc)
    case wu: WriteUniform[?] => interpretWriteUniform(wu, sc)
    case _                   => throw IllegalArgumentException("interpretOne: invalid GIO")

  @annotation.tailrec
  private def interpretMany(gios: List[GIO[?]], sc: SimContext): SimContext = gios match
    case FlatMap(gio, next) :: tail => interpretMany(gio :: next :: tail, sc)
    case Repeat(n, f) :: tail       =>
      val SimRes(i, _, _) = Simulate.sim(n) // just Int32, no reads/writes
      val int = i.asInstanceOf[Int]
      val newGios = (0 until int).map(i => f(i)).toList
      interpretMany(newGios ::: tail, sc)
    case head :: tail =>
      val newSc = interpretOne(head, sc)
      interpretMany(tail, newSc)
    case Nil => sc

  def interpret(gio: GIO[?], invocId: Int) = (invocId, interpretMany(List(gio), SimContext()))
  def interpret(gio: GIO[?], invocIds: List[Int]): List[(Int, SimContext)] = invocIds.map(interpret(gio, _))
