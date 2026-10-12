---
sidebar_position: 2
---

# Getting Started

:::note
Cyfra is in an early beta version. We encourage you to experiment with it and use it for non-mission-critical tasks.
However, it may not work on some setups and may suffer from issues with stability. We are working on it.
Please report all the issues you face at [our github issue tracker](https://github.com/ComputeNode/cyfra/issues).
:::

This guide will help you set up Cyfra and run your first GPU computation in minutes.

## Requirements

Cyfra requires a system with Vulkan support. Most modern GPUs from NVIDIA, AMD, and Intel work out of the box. You'll also need JDK 21 or later.

## Installation

Add Cyfra to your `build.sbt` or other build tool:

```scala
libraryDependencies ++= Seq(
  "io.computenode" %% "cyfra-foton" % "0.1.0-RC1" 
)
```

<details>
  <summary>MacOS required dependencies</summary>

  On macOS, you need to explicitly specify 2 extra dependencies:

  ```scala
  libraryDependencies ++= Seq(
    "io.computenode" %% "cyfra-foton" % "0.1.0-RC1",
    "org.lwjgl" % "lwjgl" % "3.4.0" classifier "natives-macos-arm64",
    "org.lwjgl" % "lwjgl-vulkan" % "3.4.0" classifier "natives-macos-arm64",
    "org.lwjgl" % "lwjgl-vma" % "3.4.0" classifier "natives-macos-arm64",
  )
  ```
</details>

## Scala-CLI - GPU scripting

Create a new scala file and and paste:
```scala
//> using scala "3.6.4"
//> using dep "io.computenode::cyfra-foton:0.1.0-RC1"
```

Then write your Cyfra code! Example:

```scala
//> using scala "3.6.4"
//> using dep "io.computenode::cyfra-foton:0.1.0-RC1"

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

Then you can run it with:
```
scala filename.scala
```

<details>
  <summary>MacOS required dependencies</summary>

  On macOS, you need to explicitly specify 3 extra dependencies:

  ```scala
  //> using scala "3.6.4"
  //> using dep "io.computenode::cyfra-foton:0.1.0-RC1"
  //> using dep "org.lwjgl:lwjgl:3.4.0,classifier=natives-macos-arm64"
  //> using dep "org.lwjgl:lwjgl-vulkan:3.4.0,classifier=natives-macos-arm64"
  //> using dep "org.lwjgl:lwjgl-vma:3.4.0,classifier=natives-macos-arm64"

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
</details>


## Validation layers

To use vulkan validation layers with your runs, install [Vulkan SDK](https://vulkan.lunarg.com/sdk/home).

Note that set-up on MacOS requires additional steps.