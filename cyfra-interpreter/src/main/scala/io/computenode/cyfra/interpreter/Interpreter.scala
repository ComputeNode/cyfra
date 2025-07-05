package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*
import izumi.reflect.Tag

case class Write(buffer: GBuffer[?], index: Int, value: Any)
case class Read(buffer: GBuffer[?], index: Int)
case class SimulationResult(invocs: List[InvocationSimResult])
case class InvocationSimResult(invocId: Int, instructions: List[Expression[?]], writes: List[Write], read: List[Read])

case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]

val bufferA = SimGBuffer[Int32]()
val gbuffers = Map[GBuffer[?], Array[Int32]](bufferA -> Array.fill(1024)(0))
val expression = bufferA.read(0) + 2
// val res = Simulate.sim(expression, 1024) -> SimulationResult(???)
