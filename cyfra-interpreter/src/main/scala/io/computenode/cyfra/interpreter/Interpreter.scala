package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import Result.Result
import izumi.reflect.Tag

case class InvocResult(
  invocId: Int,
  instructions: List[Expression[?]] = Nil,
  values: List[Result] = Nil,
  writes: List[Writes] = Nil,
  reads: List[Reads] = Nil,
)

case class InterpretResult(invocs: List[InvocResult] = Nil)

object Interpreter:
  private def interpretPure(gio: Pure[?], sc: SimContext): SimContext = gio match
    case Pure(value) => sc

  private def interpretWriteBuffer(gio: WriteBuffer[?], sc: SimContext): SimContext = gio match
    case WriteBuffer(buffer, index, value) =>
      val (n, _) = Simulate.sim(index, SimContext()) // Int32, no reads/writes here, don't need resulting context
      val i = n.asInstanceOf[Int]
      val (res, sc1) = Simulate.sim(value, sc)
      sc1.addWrite(WriteBuf(buffer, i, res))

  private def interpretWriteUniform(gio: WriteUniform[?], sc: SimContext): SimContext = gio match
    case WriteUniform(uniform, value) => ??? // simulate value, then sc.addWrite(WriteUni...)

  private def interpretOne(gio: GIO[?], sc: SimContext): SimContext = gio match
    case p: Pure[?]          => interpretPure(p, sc)
    case wb: WriteBuffer[?]  => interpretWriteBuffer(wb, sc)
    case wu: WriteUniform[?] => interpretWriteUniform(wu, sc)
    case _                   => throw IllegalArgumentException("interpretOne: invalid GIO")

  @annotation.tailrec
  private def interpretMany(gios: List[GIO[?]], sc: SimContext): SimContext = gios match
    case FlatMap(gio, next) :: tail => interpretMany(gio :: next :: tail, sc)
    case Repeat(n, f) :: tail       =>
      val (i, _) = Simulate.sim(n, SimContext()) // just Int32, no reads/writes
      val int = i.asInstanceOf[Int]
      val newGios = (0 until int).map(i => f(i)).toList
      interpretMany(newGios ::: tail, sc)
    case head :: tail =>
      val newSc = interpretOne(head, sc)
      interpretMany(tail, newSc)
    case Nil => sc

  def interpret(gio: GIO[?], invocId: Int): InvocResult =
    val sc = interpretMany(List(gio), SimContext())
    InvocResult(invocId, ???, sc.values, sc.writes, sc.reads)

  def interpret(gio: GIO[?], invocIds: List[Int]): InterpretResult = InterpretResult(invocIds.map(interpret(gio, _)))
