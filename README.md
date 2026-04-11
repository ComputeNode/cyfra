# Cyfra

Library provides a way to compile Scala 3 DSL to SPIR-V and to run it with Vulkan runtime on GPUs.

It is multiplatform. It works on:

- Linux, Windows, and Mac (for Mac requires installation of moltenvk).
- Any dedicated or integrated GPUs that support Vulkan. In practice, it means almost all moderately modern devices from
  most manufacturers including Nvidia, AMD, Intel, Apple.

## Animations

Included Foton library provides a clean and fun way to animate functions and ray traced scenes.

## Examples

### Simple example

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

Run it with:
```
scala filename.scala
```
<details>
<summary>Running on macOS</summary>
  On macOS, you may need to add extra dependencies:
  
  ```scala
  //> using dep "org.lwjgl:lwjgl:3.4.0,classifier=natives-macos-arm64"
  //> using dep "org.lwjgl:lwjgl-vulkan:3.4.0,classifier=natives-macos-arm64"
  //> using dep "org.lwjgl:lwjgl-vma:3.4.0,classifier=natives-macos-arm64"
  ```
</details>

### Ray traced animation

![output](https://github.com/user-attachments/assets/3eac9f7f-72df-4a5d-b768-9117d651c78d)

[code](https://github.com/ComputeNode/cyfra/blob/50aecea/cyfra-examples/src/main/scala/io/computenode/samples/cyfra/foton/AnimatedRaytrace.scala)
(this is API usage, to see ray tracing implementation look
at [RtRenderer](https://github.com/ComputeNode/cyfra/blob/50aecea132188776021afe0b407817665676a021/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/RtRenderer.scala))

### Animated Julia set

<img src="assets/julia.gif" width="360">

[code](https://github.com/ComputeNode/cyfra/blob/50aecea132188776021afe0b407817665676a021/cyfra-examples/src/main/scala/io/computenode/samples/cyfra/foton/AnimatedJulia.scala)

## Animation features examples

### Custom animated functions

<img src="https://github.com/user-attachments/assets/1030d968-014a-4c2c-8f21-26b999fe57fc" width="650">

### Animated ray traced scene

<img src="https://github.com/user-attachments/assets/a4189bc3-e2a9-4e52-9363-93f83b530595" width="750">

## Coding features examples

### Case classes as GPU structs

<img src="https://github.com/user-attachments/assets/e5e2020d-fcd3-4651-b0e2-fc45567df37b" width="550">

### GSeq

<img src="https://github.com/scalag/scalag/assets/4761866/bc91caf3-d9bd-4eb7-940b-be278d928243" width="600">


<img src="https://github.com/scalag/scalag/assets/4761866/2791afd8-0b3e-4113-8e01-3f4efccab37f" width="750">

## Development

To enable validation layers for vulkan, you need to install vulkan SDK. After installing, set the following VM option:

```
-Dio.computenode.cyfra.vulkan.validation=true
```

If you are on MacOs, then also add:

```
-Dorg.lwjgl.vulkan.libname=libvulkan.1.dylib
-Djava.library.path=$VULKAN_SDK/lib
```
