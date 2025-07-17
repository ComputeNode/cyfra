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
  def interpretPure(gio: Pure[?], invocId: Int)(using sc: SimContext): InterpretResult = gio match
    case Pure(value) =>
      InterpretResult:
        List(InvocResult(invocId, ???, List(value), ???, sc.reads))

  def interpretWriteBuffer(gio: WriteBuffer[?], invocId: Int)(using sc: SimContext): InterpretResult = gio match
    case WriteBuffer(buffer, index, value) =>
      val i = Simulate.sim(index).asInstanceOf[Int] // reads happening here?
      val res = Simulate.sim(value) // reads happening here? (has its own SimContext)
      // sc.bufMap(buffer)(i) = res
      val writes = List(Write(buffer, i, res))
      val reads = ??? // get all the reads from the SimContext?
      InterpretResult:
        List(InvocResult(invocId, ???, List(value), writes, reads))

  def interpretWriteUniform(gio: WriteUniform[?], invocId: Int)(using sc: SimContext): InterpretResult = gio match
    case WriteUniform(uniform, value) => ???

  def interpretOne(gio: GIO[?])(using SimContext): InterpretResult =
    val invocId = Simulate.sim(invocationId).asInstanceOf[Int]
    gio match
      case p: Pure[?]          => interpretPure(p, invocId)
      case wb: WriteBuffer[?]  => interpretWriteBuffer(wb, invocId)
      case wu: WriteUniform[?] => interpretWriteUniform(wu, invocId)
      case _                   => throw IllegalArgumentException("interpretOne: invalid GIO")

  @annotation.tailrec
  def interpretMany(gios: List[GIO[?]], res: InterpretResult)(using SimContext): InterpretResult = gios match
    case FlatMap(gio, next) :: tail => interpretMany(gio :: next :: tail, res)
    case Repeat(n, f) :: tail       =>
      val int = Simulate.sim(n).asInstanceOf[Int]
      val newGios = (0 until int).map(i => f(i)).toList
      interpretMany(newGios ::: tail, res)
    case head :: tail => interpretMany(tail, res.add(interpretOne(head)))
    case Nil          => res

  def interpret(gio: GIO[?])(using SimContext): InterpretResult = interpretMany(List(gio), InterpretResult())
