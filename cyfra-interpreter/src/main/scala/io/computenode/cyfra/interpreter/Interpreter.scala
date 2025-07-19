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
)

case class InterpretResult(invocs: List[InvocResult] = Nil):
  def add(that: InterpretResult) = InterpretResult(that.invocs ::: invocs)

object Interpreter:
  def interpretPure(gio: Pure[?], invocId: Int, sc: SimContext): InvocResult = gio match
    case Pure(value) => InvocResult(invocId, ???, List(value), ???, ???)

  def interpretWriteBuffer(gio: WriteBuffer[?], invocId: Int, sc: SimContext): InvocResult = gio match
    case WriteBuffer(buffer, index, value) =>
      val (n, _) = Simulate.sim(index)
      val i = n.asInstanceOf[Int]
      val (res, _) = Simulate.sim(value)
      val newSc = sc.addWrite(buffer, i, res)
      InvocResult(invocId, ???, List(value), ???, ???)

  def interpretWriteUniform(gio: WriteUniform[?], invocId: Int, sc: SimContext): InvocResult = gio match
    case WriteUniform(uniform, value) => ???

  def interpretOne(gio: GIO[?], invocId: Int, sc: SimContext): InvocResult =
    // val invocId = Simulate.sim(invocationId, sc, Map()).asInstanceOf[Int]
    gio match
      case p: Pure[?]          => interpretPure(p, invocId, sc)
      case wb: WriteBuffer[?]  => interpretWriteBuffer(wb, invocId, sc)
      case wu: WriteUniform[?] => interpretWriteUniform(wu, invocId, sc)
      case _                   => throw IllegalArgumentException("interpretOne: invalid GIO")

  @annotation.tailrec
  def interpretMany(gios: List[GIO[?]], invocId: Int, res: InvocResult, sc: SimContext): InvocResult = gios match
    case FlatMap(gio, next) :: tail => interpretMany(gio :: next :: tail, invocId, res, sc)
    case Repeat(n, f) :: tail       =>
      val (i, _) = Simulate.sim(n)
      val int = i.asInstanceOf[Int]
      val newGios = (0 until int).map(i => f(i)).toList
      interpretMany(newGios ::: tail, invocId, res, sc)
    case head :: tail =>
      val newRes = interpretOne(head, invocId, sc)
      interpretMany(tail, invocId, ???, sc)
    case Nil => res

  def interpret(gio: GIO[?], invocId: Int): InvocResult = interpretMany(List(gio), invocId, InvocResult(invocId), SimContext())
