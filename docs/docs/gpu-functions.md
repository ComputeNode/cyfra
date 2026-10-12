---
sidebar_position: 3
---

# GPU Functions

The simplest way to use the Cyfra library is with a GFunction. In essence, it is a function that takes any input you give it, runs on the GPU, and returns the output.

```scala
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime

@main
def multiplyByTwo(): Unit =
  VkCyfraRuntime.using:
    val input = (0 until 256).map(_.toFloat).toArray

    val doubleIt: GFunction[GStruct.Empty, Float32, Float32] = GFunction: x =>
      x * 2.0f

    val result: Array[Float] = doubleIt.run(input)

    println(s"Output: ${result.take(10).mkString(", ")}...")
```

`doubleIt.run(input)` will simply take the provided input and run the GFunction on it. As a result, we will get an array of floats that are each a doubled entry from the input array.

## Cyfra DSL

When you use Cyfra, you enter a world of values that are entirely separate from standard Scala values. Float becomes `Float32`, Double becomes `Float64`, and so on. Below is a table with more examples. Those are not all the types, but this includes most important ground types (`Float32`, `Int32`, `GBoolean`, etc.). Any ground types can be then used with Vectors. Additionally any types can be composed into `GStruct`s (including other `GStruct`s).

| Scala Type | Cyfra Type |
|------------|------------|
| `Float` | `Float32` |
| `Double` | `Float64` |
| `Int` | `Int32` |
| `Int` | `UInt32` (unsigned) |
| `Boolean` | `GBoolean` |
| `(Float, Float)` | `Vec2[Float32]` |
| `(Float, Float, Float, Float)` | `Vec4[Float32]` | 
| `(Int, Int)` | `Vec2[Int32]` |

The operators you use stay the same, but keep in mind - for an operation to happen on the GPU, it needs to involve a Cyfra value type.

## Using Uniforms

In the previous example, the GFunction only took a float array as an input. There is, however, a way to provide additional parameters to each run. This has to do with the first type parameter of `GFunction` that was set to `GStruct.Empty` in the previous example. This is the Uniform structure that can be provided for each GFunction.

```scala
case class FunctionParam(a: Float32) extends GStruct[FunctionParam]

@main
def multiplyByTwo(): Unit =
  VkCyfraRuntime.using:
    val input = (0 until 256).map(_.toFloat)

    val doubleIt: GFunction[FunctionParam, Float32, Float32] = GFunction: 
      (params: FunctionParam, x: Float32) =>
        x * params.a

    val params = FunctionParam(2.0f)
    
    val result: Array[Float] = doubleIt.run(input, params)

    println(s"Output: ${result.take(10).mkString(", ")}...")
```

You can see that the lambda in GFunction takes `FunctionParam`. The GStruct case class can be any product of any Cyfra values (including other structs).

## If-else becomes when-otherwise

Because in Cyfra we live in a different (GPU) world, it is required to use alternative control expressions. The most basic one is the `when`(-`elseWhen`-)`otherwise`:
```scala
val multiplyIt: GFunction[FunctionParam, Float32, Float32] = GFunction: 
  (params: FunctionParam, x: Float32) =>
    when(x < 100f):
      x * params.a
    .elseWhen(x < 200f):
      x * params.a * 2f
    .otherwise:
      x * params.a * 4f
```

## GSeqs

To iterate and express collections, Cyfra offers a `GSeq` type. It corresponds to a `LazyList` from Scala - a lazily evaluated sequence that can be transformed and consumed with familiar functional operations.

### Creating a GSeq

Use `GSeq.gen` to create a sequence by providing an initial value and a function that produces the next element:

```scala
// Create from a known list of elements
val colors = GSeq.of(List(red, green, blue))

// Generate integers: 0, 1, 2, 3, ...
val integers = GSeq.gen[Int32](0, n => n + 1)

// Generate Fibonacci-like pairs using Vec2: (0,1), (1,1), (1,2), (2,3), ...
val fibonacci = GSeq.gen[Vec2[Float32]]((0.0f, 1.0f), pair => (pair.y, pair.x + pair.y))

// Mandelbrot iteration: z = z² + c
val mandelbrot = GSeq.gen(
  vec2(0.0f, 0.0f), 
  z => vec2(z.x * z.x - z.y * z.y + cx, 2.0f * z.x * z.y + cy)
)
```

You must always call `.limit(n)` before consuming a GSeq to set a maximum iteration count (infinite sequences are not supported on GPU).

### Map, filter, takeWhile

Transform and filter sequences with familiar operations:

```scala
// Map: transform each element
val doubled = GSeq.gen[Int32](0, _ + 1).limit(100).map(_ * 2)

// Filter: keep only matching elements
val evens = GSeq.gen[Int32](0, _ + 1).limit(100).filter(n => n.mod(2) === 0)

// TakeWhile: stop when condition becomes false
val underTen = GSeq.gen[Int32](0, _ + 1).limit(100).takeWhile(_ < 10)
```

These can be chained together:

```scala
// Julia set iteration: iterate until escape or limit
val iterations = GSeq
  .gen(uv, v => ((v.x * v.x) - (v.y * v.y), 2.0f * v.x * v.y) + const)
  .limit(1000)
  .map(length)           // Transform to magnitude
  .takeWhile(_ < 2.0f)   // Stop when magnitude exceeds 2
```

### Fold, count, lastOr

Terminal operations consume the sequence and produce a result:

```scala
// Count: number of elements that passed through
val iterationCount: Int32 = GSeq
  .gen(vec2(0f, 0f), z => vec2(z.x*z.x - z.y*z.y + cx, 2f*z.x*z.y + cy))
  .limit(256)
  .takeWhile(z => z.x*z.x + z.y*z.y < 4.0f)
  .count

// Fold: reduce with accumulator
val sum: Int32 = GSeq.gen[Int32](1, _ + 1).limit(10).fold(0, _ + _)

// LastOr: get final element (or default if empty)
val finalValue: Int32 = GSeq.gen[Int32](0, _ + 1).limit(10).lastOr(0)
```

**Every GSeq must have a hard `limit` of maximum elements it can hold**


## Example usage

GFunction may be a simple construct, but it is enough to accelerate many applications. An example is a raytracer that would otherwise take a very long time to run on a CPU. Here is the implementation of a raytracer with Cyfra:

![Animated Raytracing](https://github.com/user-attachments/assets/3eac9f7f-72df-4a5d-b768-9117d651c78d)

Source:
 - [ImageRtRenderer.scala](https://github.com/ComputeNode/cyfra/blob/cab6b4cae3a3402a3de43272bc7cb50acf5ec67b/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/ImageRtRenderer.scala)
 - [RtRenderer.scala](https://github.com/ComputeNode/cyfra/blob/cab6b4cae3a3402a3de43272bc7cb50acf5ec67b/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/RtRenderer.scala)
