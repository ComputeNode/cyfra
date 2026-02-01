# AGENTS.md - Cyfra Development Guide

## Project Overview

Cyfra is a Scala 3 library for GPU computing that compiles a Scala DSL to SPIR-V and executes it via Vulkan. It supports Linux, Windows, and macOS (with MoltenVK).

**Key modules:**
- `cyfra-dsl` - Core DSL for GPU computations (values, expressions, bindings, structs)
- `cyfra-compiler` - Compiles DSL to SPIR-V bytecode
- `cyfra-vulkan` - Low-level Vulkan bindings via LWJGL
- `cyfra-runtime` - Runtime execution via VkCyfraRuntime
- `cyfra-core` - GProgram abstraction connecting DSL and runtime
- `cyfra-foton` - High-level GFunction API, image/animation rendering, ray tracing
- `cyfra-fluids` - 3D fluid simulation example
- `cyfra-analytics` - Customer segmentation server (http4s/tapir + GPU)
- `cyfra-fs2` - fs2 streaming interop (GPipe, GCluster)
- `cyfra-llama` - LLM inference with F16/F32 pipelines
- `cyfra-spirv-tools` - SPIR-V validation/optimization/disassembly
- `cyfra-utility` - Logging, utility functions
- `cyfra-e2e-test` - End-to-end tests
- `cyfra-examples` - Example programs

## Essential Commands

### Build & Compile
```bash
sbt compile                   # Compile all modules
sbt "project dsl" compile     # Compile specific module
```

### Testing
```bash
sbt test                      # Run all tests (requires Vulkan-capable GPU)
sbt "project fluids" test     # Test specific module
sbt "project e2eTest" test    # E2E tests (forked, custom JVM options)
```

### Formatting
```bash
sbt formatAll                 # Format all code (scalafmt)
sbt formatCheckAll            # Check formatting (CI uses this)
```

### Running Examples
```bash
sbt "project examples" run    # Run examples
sbt "project analytics" run   # Start segmentation server (port 8081)
sbt "project fluids" run      # Run fluid simulation
sbt "project llama" run       # Run LLM inference (requires model file)
```

### Llama Runner
```bash
# From sbt:
sbt "project llama" "run --model models/Llama-3.2-1B-Instruct-f16.gguf -i"
sbt "project llama" "run -m model.gguf --measure -n 128"  # Benchmark
```

## Code Organization

```
io.computenode.cyfra
├── dsl/                    # DSL types and abstractions
│   ├── Value.*             # Int32, Float32, Float16, Vec3, Vec4, etc.
│   ├── Expression.*        # Expression tree nodes
│   ├── algebra/            # ScalarAlgebra, VectorAlgebra (givens)
│   ├── binding/            # GBuffer, GUniform, GShared
│   ├── collections/        # GArray, GArray2D, GSeq
│   ├── control/            # When (conditionals), Pure, Scope
│   ├── gio/                # GIO monad for GPU operations
│   ├── library/            # Functions, Math3D, Color, Random
│   └── struct/             # GStruct (case class → GPU struct)
├── spirv/                  # SPIR-V compiler
│   └── compilers/          # DSLCompiler, ExpressionCompiler, etc.
├── core/                   # Core abstractions
│   ├── GProgram            # GPU program definition
│   ├── GExecution          # Execution model
│   ├── GCodec              # Type serialization to ByteBuffer
│   ├── layout/Layout       # Layout derivation for program layouts
│   └── CyfraRuntime        # Runtime trait
├── runtime/                # Vulkan runtime
│   └── VkCyfraRuntime      # Main runtime implementation
├── vulkan/                 # Vulkan bindings (LWJGL)
├── foton/                  # High-level APIs
│   ├── GFunction           # Simple Array[A] => Array[B] function
│   ├── rt/                 # Ray tracing (Camera, Scene, Shape, Material)
│   └── animation/          # AnimatedFunction, AnimationRenderer
└── llama/                  # LLM inference
    ├── inference/          # LlamaInference, CPUInference
    ├── pipeline/           # LlamaF16Pipeline, LlamaF32Pipeline
    ├── programs/f16/       # F16 GPU programs (attention, matmul, etc.)
    ├── programs/f32/       # F32 GPU programs
    └── gguf/               # GGUF model loading
```

## DSL Patterns

### Importing the DSL
```scala
import io.computenode.cyfra.dsl.{*, given}          // All DSL types and givens
import io.computenode.cyfra.core.GCodec.{*, given}  // Codecs
import io.computenode.cyfra.core.layout.Layout      // Layout derivation
```

### Creating a GFunction (High-Level)
```scala
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime

given CyfraRuntime = VkCyfraRuntime()

val fn: GFunction[GStruct.Empty, Float32, Float32] = GFunction: x =>
  (x + 1.0f) * (x - 2.0f)

val result: Array[Float] = fn.run(inputArray)
```

### GPU Structs (Case Classes for Uniforms)
```scala
// Define a struct that can be passed as a uniform
case class MyParams(
  dt: Float32,
  gridSize: Int32,
) extends GStruct[MyParams]

object MyParams:
  given GStructSchema[MyParams] = GStructSchema.derived
```

### Program Layout (Buffers and Uniforms)
```scala
// Layout defines all GPU bindings for a program
case class ProgramLayout(
  input: GBuffer[Float16],
  weight: GBuffer[Float16],
  output: GBuffer[Float16],
  params: GUniform[MyParams],
) derives Layout
```

### GProgram Pattern (Low-Level)
```scala
object MyProgram:
  case class Sizes(numElements: Int, featureSize: Int)
  
  case class ProgramLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32],
    params: GUniform[MyParams],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    GProgram[Sizes, ProgramLayout](
      // Layout factory: creates buffer specs from params
      layout = s => ProgramLayout(
        input = GBuffer[Float32](s.numElements),
        output = GBuffer[Float32](s.numElements),
        params = GUniform[MyParams](),
      ),
      // Dispatch: determines workgroup count
      dispatch = (_, s) => StaticDispatch(((s.numElements + 255) / 256, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      // GPU kernel body
      val tid = GIO.invocationId
      GIO.when(tid < sizes.numElements):
        val x = GIO.read[Float32](layout.input, tid)
        val p = layout.params.read
        GIO.write[Float32](layout.output, tid, x * p.dt)
```

### GIO Operations
```scala
// Read from buffer at index
val value = GIO.read[Float32](buffer, index)
val value = buffer.read(index)  // alternative syntax

// Write to buffer (returns GIO for chaining)
GIO.write[Float32](buffer, index, value)

// Read uniform struct
val params = layout.params.read

// Get thread IDs
val globalId = GIO.invocationId           // Global invocation ID (flattened)
val localId = GIO.localInvocationId       // Local ID within workgroup (Vec3)
val workgroupId = GIO.workgroupId         // Workgroup ID (Vec3)

// Conditionals
GIO.when(condition):
  // body executed if condition true

// Barriers
GIO.barrier  // Workgroup memory barrier

// Subgroup operations (warp-level)
val sum = GIO.subgroupAdd(localValue)
val max = GIO.subgroupMax(localValue)

// Loops
GIO.repeat(iterations): i =>
  // body with iteration index i

// Fold loops (accumulate result)
GIO.foldRepeat[Float32](iterations, initialValue): (i, acc) =>
  // return new accumulator value
```

### GSeq - Lazy GPU Sequences
```scala
// Generate sequence: start value + next function
val seq = GSeq.gen[Int32](startIdx, _ + stride)
  .limit(numIterations)
  .fold(0.0f, (sum: Float32, idx: Int32) =>
    val value = GIO.read[Float32](buffer, idx)
    sum + value
  )

// With unrolling hint for small fixed loops
GSeq.gen[Int32](0, _ + 1)
  .limit(headSize)
  .unroll  // generates #pragma unroll
  .fold(0.0f, (acc, d) => acc + values(d))

// Filtering
GSeq.gen[Int32](0, _ + 1)
  .limit(maxLen)
  .takeWhile(_ < actualLen)
  .fold(...)
```

### Conditionals
```scala
// When expression (returns value)
when(condition)(
  thenValue
).otherwise(
  elseValue
)

// Chained conditions
when(cond1)(val1)
  .elseWhen(cond2)(val2)
  .otherwise(val3)

// GIO.when for side effects
GIO.when(tid < numElements):
  GIO.write(output, tid, value)
```

### Shared Memory (Workgroup Local)
```scala
// Declare shared memory buffer
val sharedMem = GShared[Float32](256)  // 256 elements

// Write to shared memory
sharedMem.write(localIdx, value)

// Read from shared memory
val v = sharedMem.read(localIdx)

// Barrier before reading what others wrote
GIO.barrier
```

### Vector Operations
```scala
val v1: Vec4[Float32] = vec4(1.0f, 2.0f, 3.0f, 4.0f)
val v2 = v1 * 2.0f + vec4(1.0f, 1.0f, 1.0f, 1.0f)
val dotProduct = v1.dot(v2)
val xyz: Vec3[Float32] = v1.xyz  // Swizzle

// Type conversions
val f16: Float16 = f32Value.asFloat16
val f32: Float32 = f16Value.asFloat32
val intVal: Int32 = floatVal.asInt
val floatVal: Float32 = intVal.asFloat
```

### Math Functions
```scala
// From io.computenode.cyfra.dsl.library.Functions
sqrt(x)
exp(x)
log(x)
sin(x), cos(x), tan(x)
abs(x)
min(a, b), max(a, b)
clamp(x, minVal, maxVal)
mix(a, b, t)  // Linear interpolation
```

## Testing Patterns

### Test Setup
```scala
class MyTest extends munit.FunSuite:
  var runtime: VkCyfraRuntime = null

  override def beforeAll(): Unit =
    runtime = VkCyfraRuntime()

  override def afterAll(): Unit =
    if runtime != null then runtime.close()

  test("description"):
    given VkCyfraRuntime = runtime
    // test code
```

### E2E Tests
- Located in `cyfra-e2e-test/src/test/scala/`
- Run forked with custom JVM options: `-Dorg.lwjgl.system.stackSize=1024`
- Buffer sizes must be multiples of 256 (Vulkan alignment)

## Style Conventions

### Scalafmt Configuration
- Max column: 150
- Scala 3 dialect
- Trailing commas: always
- Scala 3 syntax rewrites enabled
- Run `sbt formatAll` before committing

### Naming
- GPU programs: `*Program` (e.g., `AdvectionProgram`, `RMSNormProgram`)
- Program layouts: `ProgramLayout` or `*Layout` case class with `derives Layout`
- GPU struct schemas: companion object with `given GStructSchema[T] = GStructSchema.derived`
- F16 vs F32 variants: prefix with `F16*` or `F32*`
- Size parameters: `Sizes` case class with computed properties

### Code Style
- Use explicit type annotations for public APIs
- Prefer extension methods for DSL operations
- Workgroup sizes typically `(256, 1, 1)` or `(128, 1, 1)`
- WARP_SIZE constant = 32 (matches GPU subgroup size)
- Compile-time constants as vals in enclosing scope, lifted to Int32/Float32 in kernel

## Important Gotchas

### Vulkan/GPU Requirements
- Tests require a Vulkan-capable GPU
- Buffer sizes must be multiples of 256 bytes (Vulkan alignment)
- CI runs `formatCheckAll; compile` only (no GPU on CI runners)

### Runtime Lifecycle
```scala
// Always close runtime
val runtime = VkCyfraRuntime()
try
  // use runtime
finally
  runtime.close()

// Or use the helper:
VkCyfraRuntime.using:
  // runtime available as given
```

### Validation Layers (Development)
```bash
# Enable Vulkan validation layers
-Dio.computenode.cyfra.vulkan.validation=true

# macOS additional settings
-Dorg.lwjgl.vulkan.libname=libvulkan.1.dylib
-Djava.library.path=$VULKAN_SDK/lib
```

### SPIR-V Compilation
- DSL compiles to SPIR-V at runtime via `DSLCompiler.compile()`
- Use `cyfra-spirv-tools` for validation/debugging
- `SpirvDisassembler` outputs human-readable SPIR-V assembly

### ByteBuffer Handling
- Use `ByteOrder.nativeOrder()` for all GPU buffers
- GCodec handles serialization: `codec.toByteBuffer(buffer, array)`
- Always `rewind()` buffers before passing to GPU

### Numeric Precision
- F16 programs use F32 for intermediate computations (numerical stability)
- Use `.asFloat32` / `.asFloat16` for conversions
- Subgroup operations (subgroupAdd, subgroupMax) are hardware-accelerated

### GSeq Limitations
- Must have `.limit(n)` before `.fold()` - infinite streams not supported
- Use `.unroll` only for small, fixed-size loops

## Module Dependencies

```
utility
├── spirvTools
├── vulkan
└── dsl
    └── compiler
        └── core
            └── runtime
                ├── foton
                │   ├── fluids
                │   ├── analytics
                │   └── examples
                └── fs2interop
                    └── e2eTest
```

## CI/CD

- **CI**: `sbt "formatCheckAll; compile"` on push to main/tags and PRs to dev
- **Release**: `sbt ci-release` on version tags (v*)
- **JDK**: GraalVM Java 21 (CI), Temurin 21 (release)
- **Artifacts**: Published to Maven Central via sbt-ci-release

## Documentation

- Project docs: `docs/` directory (Docusaurus)
- API docs: In-code Scaladoc
- Examples: `cyfra-examples/src/main/scala/`

## Common Program Patterns

### Element-wise Operation
```scala
GProgram[Sizes, Layout](
  layout = s => Layout(GBuffer(s.n), GBuffer(s.n)),
  dispatch = (_, s) => StaticDispatch(((s.n + 255) / 256, 1, 1)),
  workgroupSize = (256, 1, 1),
): layout =>
  val tid = GIO.invocationId
  GIO.when(tid < sizes.n):
    val x = GIO.read(layout.input, tid)
    GIO.write(layout.output, tid, transform(x))
```

### Reduction with Subgroups
```scala
val localSum = GSeq.gen[Int32](laneId, _ + WARP_SIZE)
  .limit(numIterations)
  .fold(0.0f, (sum, k) =>
    when(k < totalSize)(sum + GIO.read(input, k))
    .otherwise(sum)
  )
val totalSum = GIO.subgroupAdd(localSum)
```

### Attention Pattern (Q·K→softmax→V)
```scala
// Phase 1: Compute scores
for
  scores <- computeQKScores
  _ <- GIO.barrier
  // Phase 2: Softmax
  maxScore <- GIO.pure(GIO.subgroupMax(localMax))
  expSum <- computeExpSum(scores, maxScore)
  _ <- GIO.barrier
  // Phase 3: Normalize
  _ <- normalizeScores(scores, expSum)
  _ <- GIO.barrier
  // Phase 4: Weighted sum of V
  _ <- computeWeightedV(scores, vCache)
yield GStruct.Empty()
```
