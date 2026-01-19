---
sidebar_position: 4
---

# GPU Programs and Memory

While `GFunction` provides a simple interface for running computations on GPU, `GProgram` and `GBufferRegion` offer more control over GPU memory management and program execution. These are the building blocks for more complex GPU workloads.

## GProgram

A `GProgram` represents a single GPU compute shader. It defines:

- **Layout** - What buffers and uniforms the program needs
- **Dispatch** - How many work groups to launch
- **Body** - The actual computation performed by each GPU thread

```scala
import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

case class DoubleLayout(
  input: GBuffer[Float32], 
  output: GBuffer[Float32]
) derives Layout

val doubleProgram: GProgram[Int, DoubleLayout] = GProgram.static[Int, DoubleLayout](
  layout = size => DoubleLayout(
    input = GBuffer[Float32](size), 
    output = GBuffer[Float32](size)
  ),
  dispatchSize = size => size,
): layout =>
  val idx = GIO.invocationId
  for
    _ <- GIO.when(idx < 256):
      for
        value <- GIO.value(layout.input.read(idx))
        _ <- layout.output.write(idx, value * 2.0f)
      yield ()
  yield ()
```

There are many elements in that snippet that may seem unfamiliar at the moment, but they will be addressed in this article:
 - What are the layout and dispatch?
 - What is GIO and its members like `invocationId`, `when`, `write`, `read`?
 - How to execute a GProgram, and how does it execute?

### Layout

The layout is a case class that contains all the buffers (`GBuffer`) and uniforms (`GUniform`) your program needs. It must derive `Layout` for Cyfra to understand its structure.

- **GBuffer[T]** - A GPU buffer that stores an array of values of type `T`
- **GUniform[T]** - A uniform value (constant for all invocations) of type `T` (must extend `GStruct`)

### Dispatch and Execution Model

GPU programs execute in parallel across many threads. The execution is organized in a hierarchy:

1. **Invocations (threads)** - Individual parallel executions of your program body
2. **Workgroups** - Groups of invocations that can share memory and synchronize
3. **Dispatch** - The total number of workgroups to launch

When you dispatch a program, the GPU runs:
```
Total invocations = workgroups.x × workgroups.y × workgroups.z × workgroupSize.x × workgroupSize.y × workgroupSize.z
```

#### GProgram.static

The simplest way to create a program. You specify how many elements to process, and Cyfra calculates the workgroup count automatically:

```scala
val program = GProgram.static[Int, MyLayout](
  layout = size => MyLayout(...),
  dispatchSize = size => size
): layout =>
  // body
```

For example, with `dispatchSize = 256` and default `workgroupSize = (128, 1, 1)`:
- Cyfra computes `workgroups = ((256 + 127) / 128, 1, 1) = (2, 1, 1)`
- Total invocations = 2 × 128 = 256 threads running in parallel

Each thread gets a unique `GIO.invocationId` from 0 to 255.

#### GProgram.apply

For full control over dispatch, use the explicit API:

```scala
import io.computenode.cyfra.core.GProgram.{StaticDispatch, DynamicDispatch}

val program = GProgram[Int, MyLayout](
  layout = size => MyLayout(...),
  dispatch = (layout, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
  workgroupSize = (256, 1, 1),
): layout =>
  // body
```

Dispatch options:
- **StaticDispatch(x, y, z)** - Fixed number of workgroups, known at compile time
- **DynamicDispatch(buffer, offset)** - Workgroup count read from a GPU buffer at runtime (for indirect dispatch)

### GIO - Mutable operations 

`GIO` represents GPU operations. Because it's a monad, you must compose operations using `for`/`yield` or `flatMap`/`map`.

```scala
layout =>
  val idx = GIO.invocationId
  val in = layout.input.read(idx)
  for
    _ <- layout.outputA.write(idx, value) // Buffer writes return GIOs
    _ <- layout.outputB.write(idx, value * 2.0f)
  yield ()
```

## GBufferRegion

Now that we understand `GProgram`, we need a way to execute it. `GBufferRegion` manages GPU memory allocation and provides a structured way to:

1. Allocate GPU buffers
2. Run programs that use those buffers
3. Read results back to CPU

```scala
import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.runtime.VkCyfraRuntime

@main
def run = VkCyfraRuntime.using:
  val size = 256
  val inputData = (0 until size).map(_.toFloat).toArray
  val results = Array.ofDim[Float](size)

  val region = GBufferRegion
    .allocate[DoubleLayout]
    .map: layout =>
      doubleProgram.execute(size, layout)

  region.runUnsafe(
    // Init values on start of the pipeline
    init = DoubleLayout(
      input = GBuffer(inputData),
      output = GBuffer[Float32](size),
    ),
    // Read results when done
    onDone = layout => layout.output.readArray(results),
  )

  println(s"Results: ${results.take(5).mkString(", ")}...")
  // Results: 0.0, 2.0, 4.0, 6.0, 8.0...
```

### How GBufferRegion Works

1. **allocate[Layout]** - Declares what buffers need to be allocated
2. **map** - Chains operations (like executing programs) on the allocated buffers
3. **runUnsafe** - Actually allocates GPU memory, runs the computation, and cleans up

The `init` parameter provides initial data for buffers:
- `GBuffer(array)` - Initialize a buffer with data from a Scala array
- `GBuffer[T](size)` - Allocate an empty buffer of the given size
- `GUniform(value)` - Initialize a uniform with a struct value

The `onDone` callback lets you read results:
- `buffer.readArray(targetArray)` - Copy GPU buffer contents to a Scala array

## Complete Example: Parameterized Program

Here's a complete example with a uniform parameter:

```scala
import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

// Define a struct for uniform parameters
case class MulParams(factor: Float32) extends GStruct[MulParams]

// Define the program layout
case class MulLayout(
  input: GBuffer[Float32], 
  output: GBuffer[Float32], 
  params: GUniform[MulParams]
) derives Layout

// Create the program
val mulProgram: GProgram[Int, MulLayout] = GProgram.static[Int, MulLayout](
  layout = size => MulLayout(
    input = GBuffer[Float32](size), 
    output = GBuffer[Float32](size), 
    params = GUniform[MulParams]()
  ),
  dispatchSize = size => size,
): layout =>
  val idx = GIO.invocationId
  for
    _ <- GIO.when(idx < 256):
      val value = layout.input.read(idx)
      val factor = layout.params.read.factor
      layout.output.write(idx, value * factor)
  yield ()

@main
def runMultiply(): Unit = VkCyfraRuntime.using:
  val size = 256
  val inputData = (0 until size).map(_.toFloat).toArray
  val results = Array.ofDim[Float](size)

  val region = GBufferRegion
    .allocate[MulLayout]
    .map: layout =>
      mulProgram.execute(size, layout)

  region.runUnsafe(
    init = MulLayout(
      input = GBuffer(inputData),
      output = GBuffer[Float32](size),
      params = GUniform(MulParams(3.0f)), // Multiply by 3
    ),
    onDone = layout => layout.output.readArray(results),
  )
```

## Running Multiple Programs

You can run multiple programs within a single `GBufferRegion.map` block. The key insight is that `.execute` returns the Layout, so you can use the output buffers from one program as input to the next.

```scala
import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

// Program 1: Double the input
case class DoubleLayout(input: GBuffer[Float32], output: GBuffer[Float32]) derives Layout

val doubleProgram = GProgram.static[Int, DoubleLayout](
  layout = size => DoubleLayout(GBuffer[Float32](size), GBuffer[Float32](size)),
  dispatchSize = size => size
): layout =>
  val idx = GIO.invocationId
  for
    _ <- GIO.when(idx < 256):
      val value = layout.input.read(idx)
      layout.output.write(idx, value * 2.0f)
  yield ()

// Program 2: Add a constant
case class AddParams(addend: Float32) extends GStruct[AddParams]
case class AddLayout(input: GBuffer[Float32], output: GBuffer[Float32], params: GUniform[AddParams]) derives Layout

val addProgram = GProgram.static[Int, AddLayout](
  layout = size => AddLayout(GBuffer[Float32](size), GBuffer[Float32](size), GUniform[AddParams]()),
  dispatchSize = size => size
): layout =>
  val idx = GIO.invocationId
  for
    _ <- GIO.when(idx < 256):
      val value = layout.input.read(idx)
      val addend = layout.params.read.addend
      layout.output.write(idx, value + addend)
  yield ()

// Combined layout with intermediate buffer
case class PipelineLayout(
  input: GBuffer[Float32],
  intermediate: GBuffer[Float32],  // Output of program 1, input of program 2
  output: GBuffer[Float32],
  addParams: GUniform[AddParams]
) derives Layout

@main
def runPipeline(): Unit = VkCyfraRuntime.using:
  val size = 256
  val inputData = (0 until size).map(_.toFloat).toArray
  val results = Array.ofDim[Float](size)

  val region = GBufferRegion
    .allocate[PipelineLayout]
    .map: layout =>
      // Program 1: input -> intermediate (doubles values)
      val afterDouble = doubleProgram.execute(size, DoubleLayout(layout.input, layout.intermediate))
      
      // Program 2: use intermediate from afterDouble as input, write to output
      addProgram.execute(size, AddLayout(afterDouble.output, layout.output, layout.addParams))
      
      // Return the original layout
      layout

  region.runUnsafe(
    init = PipelineLayout(
      input = GBuffer(inputData),
      intermediate = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      addParams = GUniform(AddParams(10.0f)),
    ),
    onDone = layout => layout.output.readArray(results),
  )
```

For more complex compositions with better type safety and reusability, see [GPU Pipelines](./gpu-pipelines.md).
