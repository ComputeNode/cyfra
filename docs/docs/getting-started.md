---
sidebar_position: 2
---

:::note
Cyfra is in an early beta version. We encourage you to experiment with it and use it for non-mission-critical tasks.
However, it may not work on some setups and may suffer from issues with stability. We are working on it.
Please report all the issues you face at [our github issue tracker](https://github.com/ComputeNode/cyfra/issues).
:::

# Getting Started

This guide will help you set up Cyfra and run your first GPU computation in minutes.

## Requirements

Cyfra requires a system with Vulkan support. Most modern GPUs from NVIDIA, AMD, and Intel work out of the box. You'll also need:

- JDK 21 or later
- sbt 1.9 or later
- Vulkan SDK installed on your system

## Installation

Add Cyfra to your `build.sbt` or other build tool:

```scala
libraryDependencies ++= Seq(
  "io.computenode" %% "cyfra-foton" % "0.1.0" 
)
```

## Validation layers

To use vulkan validation layers with your runs, install [Vulkan SDK](https://vulkan.lunarg.com/sdk/home).

Note that set-up on MacOS requires additional steps.