package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import izumi.reflect.Tag

object Interpreter:
  private def interpretPure(gio: Pure[?], sc: SimContext): SimContext = gio match
    case Pure(value) =>
      val SimContext(results, records, _) = Simulate.sim(value.asInstanceOf[Value], sc.records)(using sc.data) // TODO needs fixing
      // newSc.addResult(result)
      ???

  private def interpretWriteBuffer(gio: WriteBuffer[?], sc: SimContext): SimContext = gio match
    case WriteBuffer(buffer, index, value) =>
      val SimContext(n, _, _) = Simulate.sim(index, sc.records)(using sc.data) // Int32, no reads/writes here, don't need resulting context
      val i = n.asInstanceOf[Int]
      val SimContext(res, _, newSc) = Simulate.sim(value, sc.records)(using sc.data)
      // newSc.addWrite(WriteBuf(buffer, i, res))
      ???

  private def interpretWriteUniform(gio: WriteUniform[?], sc: SimContext): SimContext = gio match
    case WriteUniform(uniform, value) =>
      val SimContext(results, records, newSc) = Simulate.sim(value, sc.records)(using sc.data)
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
    case Repeat(n, f) :: tail       => // is this n ever a complex expression? Or just a plain int?
      val SimContext(results, _, _) = Simulate.sim(n, sc.records)(using sc.data) // just Int32, no reads/writes
      val repeat = results(0).asInstanceOf[Int]
      val newGios = (0 until repeat).map(i => f(i)).toList
      interpretMany(newGios ::: tail, sc)
    case head :: tail => interpretMany(tail, interpretOne(head, sc))
    case Nil          => sc

  def interpret(gio: GIO[?], sc: SimContext): SimContext = interpretMany(List(gio), sc)
