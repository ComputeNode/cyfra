---
sidebar_position: 2
---

# Getting Started

This guide will help you set up Cyfra and run your first GPU computation in minutes.

## Requirements

Cyfra requires a system with Vulkan support. Most modern GPUs from NVIDIA, AMD, and Intel work out of the box. You'll also need:

- JDK 21 or later
- sbt 1.9 or later
- Vulkan SDK installed on your system

## Installation

Add Cyfra to your `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "io.computenode" %% "cyfra-foton" % "0.1.0"  // Coming soon
)
```

:::note
Cyfra is not yet published to Maven Central. For now, clone the repository and publish locally:

```bash
git clone https://github.com/computenode/cyfra.git
cd cyfra
sbt publishLocal
```
:::

## Quick Start

Here's a complete example that doubles an array of numbers on the GPU:

```scala
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime

@main def quickStart(): Unit =
  // Initialize the Vulkan runtime
  given CyfraRuntime = VkCyfraRuntime()

  // Define a GPU function
  val doubleIt = GFunction[Float32, Float32] { x =>
    x * 2.0f
  }

  // Run it
  val input = Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
  val result = doubleIt.run(input)

  println(result.mkString(", "))
  // Output: 2.0, 4.0, 6.0, 8.0, 10.0

  summon[CyfraRuntime].asInstanceOf[VkCyfraRuntime].close()
```

That's it. The lambda `x => x * 2.0f` is compiled to SPIR-V, uploaded to the GPU, and executed in parallel across all elements.

## Next Steps

Now that you have Cyfra running, explore the core concepts:

- **[GPU Functions](/docs/gpu-functions)** — Learn how to write and configure GPU functions
- **[Composing GPU Programs](/docs/composing-gpu-programs)** — Build multi-pass pipelines that keep data on the GPU
- **[Examples](/docs/examples)** — See complete examples including fractals and simulations

