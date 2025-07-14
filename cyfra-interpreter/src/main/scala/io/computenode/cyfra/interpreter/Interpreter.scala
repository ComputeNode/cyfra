package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import izumi.reflect.Tag

case class Write(buffer: GBuffer[?], index: Int, value: Any)
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
  def interpretPure(gio: Pure[?])(using SimContext): InterpretResult = gio match
    case Pure(value) =>
      val id = Simulate.sim(invocationId).asInstanceOf[Int]
      val invocSimRes = InvocResult(invocId = id, values = List(value))
      InterpretResult(List(invocSimRes))

  def interpretWriteBuffer(gio: WriteBuffer[?]): InterpretResult = gio match
    case WriteBuffer(buffer, index, value) => ???

  def interpretWriteUniform(gio: WriteUniform[?]): InterpretResult = gio match
    case WriteUniform(uniform, value) => ???

  def interpretOne(gio: GIO[?])(using SimContext): InterpretResult = gio match
    case p: Pure[?]          => interpretPure(p)
    case wb: WriteBuffer[?]  => interpretWriteBuffer(wb)
    case wu: WriteUniform[?] => interpretWriteUniform(wu)
    case _                   => throw IllegalArgumentException("interpret: invalid GIO")

  @annotation.tailrec
  def interpretMany(gios: List[GIO[?]], simRes: InterpretResult)(using SimContext): InterpretResult = gios match
    case FlatMap(gio, next) :: tail => interpretMany(gio :: next :: tail, simRes)
    case Repeat(n, f) :: tail       =>
      val int = Simulate.sim(n).asInstanceOf[Int]
      val newGios = (0 until int).map(i => f(i)).toList
      interpretMany(newGios ::: tail, simRes)
    case head :: tail => interpretMany(tail, simRes.add(interpretOne(head)))
    case Nil          => simRes

  def interpret(gio: GIO[?])(using SimContext): InterpretResult = interpretMany(List(gio), InterpretResult())
