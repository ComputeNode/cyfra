---
sidebar_position: 3
---

# GPU Functions

The simplest way to use the Cyfra library is with a GFunction. In essence, it is a function that takes any input you give it, runs on the GPU, and returns the output.

```scala
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime

@main
def multiplyByTwo(): Unit =
  VkCyfraRuntime.using:
    val input = (0 until 256).map(_.toFloat).toArrayQ

    val doubleIt: GFunction[GStruct.Empty, Float32, Float32] = GFunction: x =>
      x * 2.0f

    val result: Array[Float] = doubleIt.run(input)

    println(s"Output: ${result.take(10).mkString(", ")}...")
```

`doubleIt.run(input)` will simply take the provided input and run the GFunction on it. As a result, we will get an array of floats that are each a doubled entry from the input array.

## Cyfra DSL

When you use Cyfra, you enter a world of values that are entirely separate from standard Scala values. Float becomes Float32, Double becomes Float64, and so on. Below is a table with more examples.

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
    val input = (0 until 256).map(_.toFloat).toArrayQ

    val doubleIt: GFunction[FunctionParam, Float32, Float32] = GFunction: 
      (params: FunctionParam, x: Float32) =>
        x * params.a

    val params = FunctionParam(2.0f)
    
    val result: Array[Float] = doubleIt.run(input, params)

    println(s"Output: ${result.take(10).mkString(", ")}...")
```

You can see that the lambda in GFunction takes `FunctionParam`. The GStruct case class can be any product of any Cyfra values (including other structs).

## Example usage

GFunction may be a simple construct, but it is enough to accelerate many applications. An example is a raytracer that would otherwise take a very long time to run on a CPU. Here is the implementation of a raytracer with Cyfra:

![Animated Raytracing](https://github.com/user-attachments/assets/3eac9f7f-72df-4a5d-b768-9117d651c78d)

Source:
 - [ImageRtRenderer.scala](https://github.com/ComputeNode/cyfra/blob/cab6b4cae3a3402a3de43272bc7cb50acf5ec67b/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/ImageRtRenderer.scala)
 - [RtRenderer.scala](https://github.com/ComputeNode/cyfra/blob/cab6b4cae3a3402a3de43272bc7cb50acf5ec67b/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/RtRenderer.scala)
