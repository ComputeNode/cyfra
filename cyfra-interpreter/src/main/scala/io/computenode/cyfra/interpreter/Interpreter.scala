package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}, collections.GSeq
import binding.*, Value.*, gio.GIO, GIO.*
import izumi.reflect.Tag

object Interpreter:
  case class Write(buffer: GBuffer[?], index: Int, value: Any)
  case class Read(buffer: GBuffer[?], index: Int)
  case class InvocSimResult(
    invocId: Int,
    instructions: List[Expression[?]],
    values: List[Any] = Nil,
    writes: List[Write] = Nil,
    reads: List[Read] = Nil,
  )
  case class SimResult(invocs: List[InvocSimResult] = Nil):
    def add(that: SimResult) = SimResult(that.invocs ::: invocs)

  case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]

  // val bufferA = SimGBuffer[Int32]()
  // val gbuffers = Map[GBuffer[?], Array[Int32]](bufferA -> Array.fill(1024)(0))
  // val expression = bufferA.read(0) + 2
  // val res = Simulate.sim(expression, 1024) -> SimulationResult(???)

  def interpretPure(gio: Pure[?]): SimResult = gio match
    case Pure(value) =>
      val id = Simulate.sim(invocationId).asInstanceOf[Int]
      val invocSimRes = InvocSimResult(invocId = id, instructions = ???, values = ???)
      SimResult(List(invocSimRes))

  def interpretOne(gio: GIO[?]): SimResult = gio match
    case Pure(value)                       => ???
    case WriteBuffer(buffer, index, value) => ???
    case WriteUniform(uniform, value)      => ???
    case _                                 => throw IllegalArgumentException("interpret: invalid GIO")

  @annotation.tailrec
  def interpretMany(gios: List[GIO[?]], simRes: SimResult): SimResult = gios match
    case head :: tail =>
      head match
        case FlatMap(gio, next) => interpretMany(gio :: next :: tail, simRes)
        case Repeat(n, f)       =>
          val int = Simulate.sim(n).asInstanceOf[Int]
          val newGios = (0 until int).map(i => f(i)).toList
          interpretMany(newGios ::: tail, simRes)
        case _ => interpretMany(tail, simRes.add(interpretOne(head)))
    case Nil => simRes

  def interpret(gio: GIO[?]): SimResult = interpretMany(List(gio), SimResult())
