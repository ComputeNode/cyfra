---
sidebar_position: 5
---

# GPU Pipelines with GExecution

When you need to run multiple GPU programs in sequence, sharing data between them, `GExecution` provides a composable way to build GPU pipelines. The big benefit is that those pipelines, even highly complex ones, are materialized as one on the GPU and will synchronize and pass data without any operations on the CPU side. This greatly improves performance of computations.

## What is GExecution?

`GExecution` represents a sequence of GPU operations that can be composed together. It's a monadic abstraction that allows you to:

- Chain multiple `GProgram`s together
- Share buffers between pipeline stages
- Transform parameters and layouts between programs

Every `GProgram` is also a `GExecution`, making them directly composable.

## Building a Simple Pipeline

Let's build a pipeline that doubles values and then adds a constant:

```scala
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

// Step 1: Define individual programs

case class DoubleLayout(input: GBuffer[Float32], output: GBuffer[Float32]) derives Layout

val doubleProgram: GProgram[Int, DoubleLayout] = GProgram[Int, DoubleLayout](
  layout = size => DoubleLayout(
    input = GBuffer[Float32](size), 
    output = GBuffer[Float32](size)
  ),
  dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
  workgroupSize = (256, 1, 1),
): layout =>
  val idx = GIO.invocationId
  GIO.when(idx < 256):
    val value = GIO.read(layout.input, idx)
    GIO.write(layout.output, idx, value * 2.0f)

case class AddParams(value: Float32) extends GStruct[AddParams]
case class AddLayout(
  input: GBuffer[Float32], 
  output: GBuffer[Float32], 
  params: GUniform[AddParams]
) derives Layout

val addProgram: GProgram[Int, AddLayout] = GProgram[Int, AddLayout](
  layout = size => AddLayout(
    input = GBuffer[Float32](size), 
    output = GBuffer[Float32](size), 
    params = GUniform[AddParams]()
  ),
  dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
  workgroupSize = (256, 1, 1),
): layout =>
  val idx = GIO.invocationId
  GIO.when(idx < 256):
    val value = GIO.read(layout.input, idx)
    val addValue = layout.params.read.value
    GIO.write(layout.output, idx, value + addValue)
```

## Composing Programs with addProgram

The key to building pipelines is the `addProgram` method. It takes:

1. A program to add to the execution
2. A function to map pipeline parameters to program parameters
3. A function to map the pipeline layout to the program's layout

```scala
// Step 2: Define the combined pipeline layout
case class PipelineLayout(
  input: GBuffer[Float32],
  doubled: GBuffer[Float32],   // Intermediate buffer
  output: GBuffer[Float32],
  addParams: GUniform[AddParams]
) derives Layout

// Step 3: Compose the pipeline
val doubleAndAddPipeline: GExecution[Int, PipelineLayout, PipelineLayout] =
  GExecution[Int, PipelineLayout]()
    .addProgram(doubleProgram)(
      size => size,  // Map params: pipeline size -> program size
      layout => DoubleLayout(layout.input, layout.doubled)  // Map layout
    )
    .addProgram(addProgram)(
      size => size,
      layout => AddLayout(layout.doubled, layout.output, layout.addParams)
    )
```

Notice how the `doubled` buffer connects the two programs - the first program writes to it, and the second reads from it.

## Running the Pipeline

Execute the pipeline using `GBufferRegion`:

```scala
@main
def runDoubleAndAddPipeline(): Unit = VkCyfraRuntime.using:
  val size = 256
  val inputData = (0 until size).map(_.toFloat).toArray
  val results = Array.ofDim[Float](size)

  val region = GBufferRegion
    .allocate[PipelineLayout]
    .map: layout =>
      doubleAndAddPipeline.execute(size, layout)

  region.runUnsafe(
    init = PipelineLayout(
      input = GBuffer(inputData),
      doubled = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      addParams = GUniform(AddParams(10.0f)),
    ),
    onDone = layout => layout.output.readArray(results),
  )
```

## GExecution Operations

`GExecution` provides several composition methods:

### addProgram

Add another program to the pipeline, mapping parameters and layout:

```scala
execution.addProgram(program)(
  mapParams = pipelineParams => programParams,
  mapLayout = pipelineLayout => programLayout
)
```

### map

Transform the result layout:

```scala
execution.map(resultLayout => newResultLayout)
```

### flatMap

Sequence executions where the second depends on the first's result:

```scala
execution.flatMap: resultLayout =>
  anotherExecution
```

## Example: Case-study on fs2 filtering

A great read for understanding pipelines is a report from our contributor `spamegg`. It describes a process of implementing a [parallel prefix sum](https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-39-parallel-prefix-sum-scan-cuda) based filtering approach. We highly recommend reading it: [GSoC 2025: fs2 filtering through Cyfra](https://spamegg1.github.io/gsoc-2025/#fs2-filtering-through-cyfra).

## Example: Navier-Stokes Fluid Simulation

The `cyfra-fluids` module implements a full 3D Navier-Stokes fluid solver using GExecution pipelines. Each simulation step chains multiple GPU programs: forces, advection, diffusion, pressure projection, and boundary conditions.

![Fluid Simulation](/img/full_fluid_8s.gif)

[View the implementation](https://github.com/ComputeNode/cyfra/tree/cab6b4cae3a3402a3de43272bc7cb50acf5ec67b/cyfra-fluids/src/main/scala/io/computenode/cyfra/fluids)