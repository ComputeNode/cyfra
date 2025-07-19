package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import Result.Result
import izumi.reflect.Tag

case class InvocResult(
  invocId: Int,
  instructions: List[Expression[?]] = Nil,
  values: List[Any] = Nil,
  writes: List[Write] = Nil,
  reads: List[Read] = Nil,
):
  def merge(that: InvocResult) = InvocResult(
    invocId = invocId,
    instructions = instructions ::: that.instructions,
    values = values ::: that.values,
    writes = writes ::: that.writes,
    reads = reads ::: that.reads,
  )

case class InterpretResult(invocs: List[InvocResult] = Nil):
  def add(that: InterpretResult) = InterpretResult(that.invocs ::: invocs)

object Interpreter:
  private def interpretPure(gio: Pure[?], invocId: Int, sc: SimContext): InvocResult = gio match
    case Pure(value) => InvocResult(invocId, ???, List(value), sc.writes, sc.reads)

  private def interpretWriteBuffer(gio: WriteBuffer[?], invocId: Int, sc: SimContext): InvocResult = gio match
    case WriteBuffer(buffer, index, value) =>
      val (n, _) = Simulate.sim(index)
      val i = n.asInstanceOf[Int]
      val (res, _) = Simulate.sim(value)
      val newSc = sc.addWrite(buffer, i, res) // should we keep this around?
      InvocResult(invocId, ???, List(value), newSc.writes, newSc.reads)

  private def interpretWriteUniform(gio: WriteUniform[?], invocId: Int, sc: SimContext): InvocResult = gio match
    case WriteUniform(uniform, value) => ???

  private def interpretOne(gio: GIO[?], invocId: Int, sc: SimContext): InvocResult = gio match
    case p: Pure[?]          => interpretPure(p, invocId, sc)
    case wb: WriteBuffer[?]  => interpretWriteBuffer(wb, invocId, sc)
    case wu: WriteUniform[?] => interpretWriteUniform(wu, invocId, sc)
    case _                   => throw IllegalArgumentException("interpretOne: invalid GIO")

  @annotation.tailrec
  private def interpretMany(gios: List[GIO[?]], invocId: Int, res: InvocResult, sc: SimContext): InvocResult = gios match
    case FlatMap(gio, next) :: tail => interpretMany(gio :: next :: tail, invocId, res, sc)
    case Repeat(n, f) :: tail       =>
      val (i, _) = Simulate.sim(n)
      val int = i.asInstanceOf[Int]
      val newGios = (0 until int).map(i => f(i)).toList
      interpretMany(newGios ::: tail, invocId, res, sc)
    case head :: tail =>
      val newRes = interpretOne(head, invocId, sc) // should we get the updated SimContext?
      interpretMany(tail, invocId, res.merge(newRes), sc)
    case Nil => res

  def interpret(gio: GIO[?], invocId: Int): InvocResult = interpretMany(List(gio), invocId, InvocResult(invocId), SimContext())
  def interpret(gio: GIO[?], invocIds: List[Int]): InterpretResult = InterpretResult(invocIds.map(interpret(gio, _)))
