---
sidebar_position: 3
---

# GPU Functions

A `GFunction` represents a function that runs on the GPU, transforming an input array into an output array in parallel. Think of it as a GPU-accelerated `map` operation where each element is processed independently by a separate GPU thread.

## Your First GPU Function

The simplest way to create a GPU function is by passing a lambda that describes how to transform a single element:

```scala
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime

@main def helloGpu(): Unit =
  given CyfraRuntime = VkCyfraRuntime()

  val doubleIt: GFunction[GStruct.Empty, Float32, Float32] = GFunction { x =>
    x * 2.0f
  }

  val input = (0 until 256).map(_.toFloat).toArray
  val result: Array[Float] = doubleIt.run(input)

  println(s"Input:  ${input.take(5).mkString(", ")}...")
  println(s"Output: ${result.take(5).mkString(", ")}...")
  // Output: 0.0, 2.0, 4.0, 6.0, 8.0...

  summon[CyfraRuntime].asInstanceOf[VkCyfraRuntime].close()
```

This creates a function that doubles every element. The type signature `GFunction[GStruct.Empty, Float32, Float32]` indicates that it takes no configuration parameters (`GStruct.Empty`), accepts `Float32` values as input, and produces `Float32` values as output.

The entire array is processed in parallel on the GPU. Each invocation of the lambda operates on a different element, with thousands of GPU threads working simultaneously.

## Working with Vectors

Cyfra provides built-in vector types that map directly to GPU hardware. The `Vec4[Float32]` type represents a 4-component floating-point vector and supports standard operations like normalization and dot products:

```scala
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime

@main def vectorOperations(): Unit =
  given CyfraRuntime = VkCyfraRuntime()

  val normalizeVec4: GFunction[GStruct.Empty, Vec4[Float32], Vec4[Float32]] = GFunction { v =>
    normalize(v)
  }

  val dotWithX: GFunction[GStruct.Empty, Vec4[Float32], Float32] = GFunction { v =>
    val xAxis = vec4(1.0f, 0.0f, 0.0f, 0.0f)
    v.dot(xAxis)
  }

  // On the Scala side, Vec4 maps to (Float, Float, Float, Float) tuples
  val vectors: Array[(Float, Float, Float, Float)] = Array(
    (3.0f, 0.0f, 0.0f, 0.0f),
    (0.0f, 4.0f, 0.0f, 0.0f),
    (1.0f, 1.0f, 1.0f, 1.0f)
  ) ++ Array.fill(253)((1.0f, 0.0f, 0.0f, 0.0f))

  val normalized = normalizeVec4.run(vectors)
  println(f"(3,0,0,0) normalized: (${normalized(0)._1}%.2f, ${normalized(0)._2}%.2f, ${normalized(0)._3}%.2f, ${normalized(0)._4}%.2f)")
  // Output: (1.00, 0.00, 0.00, 0.00)

  val dots = dotWithX.run(vectors)
  println(s"(3,0,0,0) dot X-axis: ${dots(0)}")
  // Output: 3.0

  summon[CyfraRuntime].asInstanceOf[VkCyfraRuntime].close()
```

The codec system handles conversion between Scala tuples and GPU vector types automatically.

## Configuration with Structs

Many GPU functions need configuration parameters that remain constant across all elements. Instead of hardcoding values, you can define a configuration struct that gets passed to the GPU as a uniform buffer.

Define your configuration as a case class extending `GStruct`:

```scala
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime

case class TransformConfig(scale: Float32, offset: Float32) extends GStruct[TransformConfig]

@main def configuredTransform(): Unit =
  given CyfraRuntime = VkCyfraRuntime()

  val transform: GFunction[TransformConfig, Float32, Float32] = 
    GFunction.forEachIndex[TransformConfig, Float32, Float32] { (config, idx, buffer) =>
      buffer.read(idx) * config.scale + config.offset
    }

  val data = (0 until 256).map(_.toFloat).toArray

  // Same compiled GPU code, different configurations
  val doubled = transform.run(data, TransformConfig(2.0f, 0.0f))
  println(s"f(x) = x * 2:      ${doubled.take(5).mkString(", ")}...")
  // Output: 0.0, 2.0, 4.0, 6.0, 8.0...

  val shifted = transform.run(data, TransformConfig(1.0f, 10.0f))
  println(s"f(x) = x + 10:     ${shifted.take(5).mkString(", ")}...")
  // Output: 10.0, 11.0, 12.0, 13.0, 14.0...

  val combined = transform.run(data, TransformConfig(0.5f, 100.0f))
  println(s"f(x) = 0.5x + 100: ${combined.take(5).mkString(", ")}...")
  // Output: 100.0, 100.5, 101.0, 101.5, 102.0...

  summon[CyfraRuntime].asInstanceOf[VkCyfraRuntime].close()
```

The `forEachIndex` variant gives you access to the configuration struct, the element index, and the input buffer. The same compiled shader can be reused with different parameter values without recompilation.

## 2D Processing

Image processing and grid-based computations work naturally when you compute 2D coordinates from the linear index. Here's a complete example that generates a Julia set fractal:

```scala
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.{GFunction, ImageUtility}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.nio.file.Paths

case class JuliaConfig(width: Int32, height: Int32, cReal: Float32, cImag: Float32) 
  extends GStruct[JuliaConfig]

@main def juliaFractal(): Unit =
  given CyfraRuntime = VkCyfraRuntime()

  val MaxIterations = 256
  val width = 512
  val height = 512

  val julia: GFunction[JuliaConfig, Int32, Vec4[Float32]] = 
    GFunction.forEachIndex[JuliaConfig, Int32, Vec4[Float32]] { (config, idx, buffer) =>
      val pixelIdx = buffer.read(idx)
      val px = pixelIdx.mod(config.width)
      val py = pixelIdx / config.width
      
      val zx = px.asFloat / config.width.asFloat * 3.0f - 1.5f
      val zy = py.asFloat / config.height.asFloat * 3.0f - 1.5f
      
      val iterations = GSeq
        .gen(vec2(zx, zy), z => 
          vec2(z.x * z.x - z.y * z.y + config.cReal, 2.0f * z.x * z.y + config.cImag)
        )
        .limit(MaxIterations)
        .takeWhile(z => z.x * z.x + z.y * z.y < 4.0f)
        .count
      
      val t = iterations.asFloat / MaxIterations.toFloat
      vec4(t * t, t, sqrt(t), 1.0f)
    }

  val indices = (0 until width * height).toArray
  val config = JuliaConfig(width, height, -0.7f, 0.27015f)

  println(s"Computing ${width}x${height} Julia set on GPU...")
  val colors: Array[(Float, Float, Float, Float)] = julia.run(indices, config)

  ImageUtility.renderToImage(colors, width, height, Paths.get("julia.png"))
  println("Saved to julia.png")

  summon[CyfraRuntime].asInstanceOf[VkCyfraRuntime].close()
```

Each pixel is calculated independently on the GPU. The `GSeq` abstraction provides a functional way to express iterative computations that compile to efficient GPU loops.

