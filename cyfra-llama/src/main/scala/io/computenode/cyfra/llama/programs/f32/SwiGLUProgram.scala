package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** SiLU (Sigmoid Linear Unit) activation function.
  * 
  * SiLU(x) = x * sigmoid(x) = x / (1 + exp(-x))
  * 
  * Also known as Swish. Used in Llama's MLP layers.
  */
object SiLUProgram:
  val BLOCK_SIZE = 512

  case class Sizes(size: Int)

  case class ProgramLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val size = sizes.size
    
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float32](s.size),
        output = GBuffer[Float32](s.size),
      ),
      dispatch = (_, s) => StaticDispatch(((s.size + BLOCK_SIZE - 1) / BLOCK_SIZE, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val sizeVal: Int32 = size
      
      GIO.when(idx < sizeVal):
        val x = GIO.read[Float32](layout.input, idx)
        val result = x / (1.0f + exp(-x))
        GIO.write[Float32](layout.output, idx, result)

/** SwiGLU activation used in Llama MLP.
  * 
  * SwiGLU(gate, up) = SiLU(gate) * up
  */
object SwiGLUProgram:
  val BLOCK_SIZE = 512

  case class Sizes(size: Int)

  case class ProgramLayout(
    gate: GBuffer[Float32],
    up: GBuffer[Float32],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val size = sizes.size
    
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        gate = GBuffer[Float32](s.size),
        up = GBuffer[Float32](s.size),
        output = GBuffer[Float32](s.size),
      ),
      dispatch = (_, s) => StaticDispatch(((s.size + BLOCK_SIZE - 1) / BLOCK_SIZE, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val sizeVal: Int32 = size
      
      GIO.when(idx < sizeVal):
        val g = GIO.read[Float32](layout.gate, idx)
        val u = GIO.read[Float32](layout.up, idx)
        val silu_g = g / (1.0f + exp(-g))
        GIO.write[Float32](layout.output, idx, silu_g * u)
