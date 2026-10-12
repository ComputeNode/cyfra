---
sidebar_position: 6
---

# There is more

Just like the Cyfra library, this documentation is a work in progress. There are many components of Cyfra that are already deep in development that are not yet documented. For the curious ones, we recommend looking at various examples of those components in our codebase:

## FS2 Integration

Stream processing with GPU acceleration using fs2 pipes:
- [GPipe](https://github.com/ComputeNode/cyfra/blob/main/cyfra-fs2/src/main/scala/io/computenode/cyfra/fs2interop/GPipe.scala) - GPU-accelerated fs2 pipes for map, filter, and batch operations
- [GCluster](https://github.com/ComputeNode/cyfra/blob/main/cyfra-fs2/src/main/scala/io/computenode/cyfra/fs2interop/GCluster.scala) - Fuzzy C-Means clustering on GPU streams

## Foton Animation Library

Create GPU-accelerated animations and ray-traced scenes:
- [AnimatedFunction](https://github.com/ComputeNode/cyfra/blob/main/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/animation/AnimatedFunction.scala) - Animate mathematical functions over time
- [AnimationRtRenderer](https://github.com/ComputeNode/cyfra/blob/main/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/animation/AnimationRtRenderer.scala) - Animated ray-traced scene renderer
- [RtRenderer](https://github.com/ComputeNode/cyfra/blob/main/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/RtRenderer.scala) - Core ray tracing implementation

## SPIR-V Debugging Tools


:::note
This requires an installed Vulkan SDK.
:::


Tools for working with SPIR-V bytecode. Use `SpirvToolsRunner` to process shaders with multiple tools:

```scala
import io.computenode.cyfra.spirvtools.*
import io.computenode.cyfra.spirvtools.SpirvTool.{ToFile, ToLogger}

// Configure which tools to run
val runner = SpirvToolsRunner(
  validator = SpirvValidator.Enable(throwOnFail = true),
  optimizer = SpirvOptimizer.Enable(settings = Seq(Param("-O"))),
  disassembler = SpirvDisassembler.Enable(toolOutput = ToLogger),
  crossCompilation = SpirvCross.Enable(toolOutput = ToFile("output", "shader.glsl")),
)

// Process shader bytecode
val optimizedCode = runner.processShaderCodeWithSpirvTools(shaderCode)
```

Or use individual tools directly:

```scala
// Validate SPIR-V bytecode
SpirvValidator.validateSpirv(shaderCode, SpirvValidator.Enable(throwOnFail = true))

// Disassemble to readable assembly
val assembly: Option[String] = SpirvDisassembler.disassembleSpirv(
  shaderCode, 
  SpirvDisassembler.Enable(toolOutput = ToLogger)
)

// Optimize for performance
val optimized: Option[ByteBuffer] = SpirvOptimizer.optimizeSpirv(
  shaderCode,
  SpirvOptimizer.Enable(settings = Seq(Param("-O")))
)

// Cross-compile to GLSL
val glsl: Option[String] = SpirvCross.crossCompileSpirv(
  shaderCode,
  SpirvCross.Enable(toolOutput = ToLogger)
)
```

