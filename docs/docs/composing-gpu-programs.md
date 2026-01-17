---
sidebar_position: 4
---

# Composing GPU Programs

While `GFunction` provides a convenient high-level API for array transformations, `GProgram` offers fine-grained control over GPU execution. Programs can be composed into pipelines where the output of one computation feeds into the next, all without transferring data back to the CPU.

## Anatomy of a GProgram

A `GProgram` consists of three parts: a layout defining the buffers and uniforms, a dispatch configuration determining how many GPU threads to launch, and a body containing the actual computation.

Here's a complete example with a program that doubles every element in a buffer:

```scala
import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

case class DoubleLayout(input: GBuffer[Float32], output: GBuffer[Float32]) extends Layout

@main def simpleProgram(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()

  val doubleProgram: GProgram[Int, DoubleLayout] = GProgram[Int, DoubleLayout](
    layout = size => DoubleLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size)
    ),
    dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
    workgroupSize = (256, 1, 1)
  ) { layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256) {
      val value = GIO.read(layout.input, idx)
      GIO.write(layout.output, idx, value * 2.0f)
    }
  }

  val size = 256
  val inputData = (0 until size).map(_.toFloat).toArray
  val results = Array.ofDim[Float](size)

  GBufferRegion
    .allocate[DoubleLayout]
    .map { layout =>
      doubleProgram.execute(size, layout)
    }
    .runUnsafe(
      init = DoubleLayout(
        input = GBuffer(inputData),
        output = GBuffer[Float32](size)
      ),
      onDone = layout => layout.output.readArray(results)
    )

  println(s"Input:  ${inputData.take(5).mkString(", ")}...")
  println(s"Output: ${results.take(5).mkString(", ")}...")
  // Output: 0.0, 2.0, 4.0, 6.0, 8.0...

  runtime.close()
```

The `layout` function describes what buffers the program needs, parameterized by an `Int` representing the array size. The `dispatch` function specifies how many workgroups to launch—here we divide by 256 (the workgroup size) and round up. The body uses `GIO` operations to read from input, perform the computation, and write to output.

## Adding Uniform Parameters

Programs often need configuration that stays constant during execution. Define a struct for your parameters and include it in the layout as a `GUniform`:

```scala
import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

case class MulParams(factor: Float32) extends GStruct[MulParams]

case class MulLayout(
  input: GBuffer[Float32], 
  output: GBuffer[Float32],
  params: GUniform[MulParams]
) extends Layout

@main def programWithUniforms(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()

  val mulProgram: GProgram[Int, MulLayout] = GProgram.static[Int, MulLayout](
    layout = size => MulLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[MulParams]()
    ),
    dispatchSize = size => size
  ) { layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256) {
      val value = GIO.read(layout.input, idx)
      val factor = layout.params.read.factor
      GIO.write(layout.output, idx, value * factor)
    }
  }

  val size = 256
  val inputData = (0 until size).map(_.toFloat).toArray
  val results = Array.ofDim[Float](size)

  GBufferRegion
    .allocate[MulLayout]
    .map { layout =>
      mulProgram.execute(size, layout)
    }
    .runUnsafe(
      init = MulLayout(
        input = GBuffer(inputData),
        output = GBuffer[Float32](size),
        params = GUniform(MulParams(3.0f))
      ),
      onDone = layout => layout.output.readArray(results)
    )

  println(s"Input:  ${inputData.take(5).mkString(", ")}...")
  println(s"Output (x * 3): ${results.take(5).mkString(", ")}...")
  // Output: 0.0, 3.0, 6.0, 9.0, 12.0...

  runtime.close()
```

The `GProgram.static` constructor simplifies dispatch configuration when you just need a linear dispatch based on element count.

## Building Pipelines

The real power of `GProgram` comes from composition. You can chain programs together using `GExecution`, creating pipelines where intermediate results stay on the GPU.

Here's a complete example of a pipeline that first multiplies every element by a factor, then adds a constant:

```scala
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

case class MulParams(factor: Float32) extends GStruct[MulParams]
case class AddParams(addend: Float32) extends GStruct[AddParams]

case class MulLayout(input: GBuffer[Float32], output: GBuffer[Float32], params: GUniform[MulParams]) extends Layout
case class AddLayout(input: GBuffer[Float32], output: GBuffer[Float32], params: GUniform[AddParams]) extends Layout

case class MulAddLayout(
  input: GBuffer[Float32],
  multiplied: GBuffer[Float32],
  output: GBuffer[Float32],
  mulParams: GUniform[MulParams],
  addParams: GUniform[AddParams]
) extends Layout

@main def pipelineExample(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()

  val mulProgram: GProgram[Int, MulLayout] = GProgram.static[Int, MulLayout](
    layout = size => MulLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[MulParams]()
    ),
    dispatchSize = size => size
  ) { layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256) {
      val value = GIO.read(layout.input, idx)
      val factor = layout.params.read.factor
      GIO.write(layout.output, idx, value * factor)
    }
  }

  val addProgram: GProgram[Int, AddLayout] = GProgram.static[Int, AddLayout](
    layout = size => AddLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[AddParams]()
    ),
    dispatchSize = size => size
  ) { layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256) {
      val value = GIO.read(layout.input, idx)
      val addend = layout.params.read.addend
      GIO.write(layout.output, idx, value + addend)
    }
  }

  // Compose into a pipeline: multiply then add
  val mulAddPipeline: GExecution[Int, MulAddLayout, MulAddLayout] =
    GExecution[Int, MulAddLayout]()
      .addProgram(mulProgram)(
        size => size,
        layout => MulLayout(layout.input, layout.multiplied, layout.mulParams)
      )
      .addProgram(addProgram)(
        size => size,
        layout => AddLayout(layout.multiplied, layout.output, layout.addParams)
      )

  val size = 256
  val inputData = (0 until size).map(_.toFloat).toArray
  val results = Array.ofDim[Float](size)

  GBufferRegion
    .allocate[MulAddLayout]
    .map { layout =>
      mulAddPipeline.execute(size, layout)
    }
    .runUnsafe(
      init = MulAddLayout(
        input = GBuffer(inputData),
        multiplied = GBuffer[Float32](size),
        output = GBuffer[Float32](size),
        mulParams = GUniform(MulParams(3.0f)),
        addParams = GUniform(AddParams(100.0f))
      ),
      onDone = layout => layout.output.readArray(results)
    )

  println("Pipeline: input -> multiply by 3 -> add 100")
  println(s"Input:  ${inputData.take(5).mkString(", ")}...")
  println(s"Output: ${results.take(5).mkString(", ")}...")
  // Output: 100.0, 103.0, 106.0, 109.0, 112.0...

  runtime.close()
```

Each `addProgram` call specifies how to map the pipeline's parameters to the program's parameters, and how to extract the program's layout from the pipeline's layout. The multiply program writes to `multiplied`, which the add program then reads from. The intermediate buffer never leaves the GPU.

## When to Use GProgram vs GFunction

`GFunction` is the right choice when you have a straightforward array transformation and want minimal boilerplate. It handles buffer management, dispatch configuration, and data transfer automatically.

`GProgram` becomes necessary when you need:

- Multiple passes over data without CPU round-trips
- Complex buffer layouts with multiple inputs and outputs  
- Fine control over workgroup sizes and dispatch dimensions
- Integration with existing SPIR-V shaders

The two approaches complement each other. In fact, `GFunction` is implemented on top of `GProgram` internally, adding convenience at the cost of flexibility.

