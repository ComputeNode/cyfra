# Cyfra Knowledge Base

## Table of Contents
1. [Core Concepts](#core-concepts)
2. [DSL Type System](#dsl-type-system)
3. [Layout System](#layout-system)
4. [GIO Monad](#gio-monad)
5. [GProgram Structure](#gprogram-structure)
6. [Buffer Operations](#buffer-operations)
7. [Helper Functions](#helper-functions)
8. [Best Practices](#best-practices)

---

## Core Concepts

### GPU Programming Model

Cyfra is a **Scala 3 DSL** that compiles to **SPIR-V** bytecode and executes on **Vulkan**-compatible GPUs. The key concepts are:

- **Expression Trees**: DSL code builds expression trees at compile time
- **SPIR-V Compilation**: Trees are compiled to SPIR-V shader code
- **Vulkan Execution**: Shaders run on GPU via Vulkan compute pipelines
- **Type Safety**: Compile-time validation of GPU operations

### Two Worlds: Host (CPU) vs Device (GPU)

| Host (Scala) | Device (GPU) |
|--------------|--------------|
| `Int`, `Float`, `Double` | `Int32`, `Float32`, `UInt32` |
| `Boolean` | `GBoolean` |
| Scala collections | `GSeq`, `GBuffer` |
| Mutable state | Immutable expressions |
| Control flow (`if/else`) | DSL control flow (`when/otherwise`) |

**Critical Rule**: Never mix host and device types. All GPU code must use DSL types.

---

## DSL Type System

### Scalar Types

```scala
// GPU scalar types
val x: Float32 = 1.0f       // 32-bit float
val i: Int32 = 42           // 32-bit signed integer
val u: UInt32 = 42.unsigned // 32-bit unsigned integer
val b: GBoolean = x > 0.0f  // Boolean condition
```

**Conversion Functions**:
```scala
val f: Float32 = i.asFloat  // Int32 → Float32
val i2: Int32 = f.asInt     // Float32 → Int32
val u: UInt32 = i.unsigned  // Int32 → UInt32
```

### Vector Types

```scala
// Vector construction
val v2: Vec2[Float32] = (1.0f, 2.0f)
val v3: Vec3[Float32] = (1.0f, 2.0f, 3.0f)
val v4: Vec4[Float32] = (1.0f, 2.0f, 3.0f, 4.0f)

// Or using helper functions
val v = vec3(1.0f, 2.0f, 3.0f)

// Component access
val x = v3.x
val y = v3.y
val z = v3.z

// Swizzling
val xy = v3.xy      // Vec2[Float32]
val zyx = v3.zyx    // Vec3[Float32]
```

**Vector Operations**:
```scala
val a: Vec3[Float32] = (1.0f, 2.0f, 3.0f)
val b: Vec3[Float32] = (4.0f, 5.0f, 6.0f)

// Component-wise operations
val sum = a + b         // (5, 7, 9)
val product = a * 2.0f  // (2, 4, 6)

// Vector operations
val dotProduct = a dot b        // Scalar
val crossProduct = a cross b    // Vec3
val len = length(a)             // Scalar
val normalized = normalize(a)   // Vec3
```

### GStruct - Custom GPU Types

```scala
case class Particle(
  position: Vec3[Float32],
  velocity: Vec3[Float32],
  mass: Float32,
  radius: Float32
) extends GStruct[Particle]
```

**Rules**:
- Must extend `GStruct[T]` where `T` is the case class itself
- Only GPU-compatible types as fields
- Nested structs supported
- Automatic memory layout via compile-time reflection

**Memory Layout**:
- Vec3 is padded to 16 bytes (not 12!)
- Alignment follows std430 layout rules
- Check actual sizes if memory-constrained

---

## Layout System

### What is a Layout?

A **Layout** defines the GPU memory layout for a shader program. It specifies:
- **GBuffer**: Arrays of GPU data
- **GUniform**: Read-only parameters

```scala
case class MyLayout(
  input: GBuffer[Float32],
  output: GBuffer[Vec3[Float32]],
  params: GUniform[MyParams]
) extends Layout
```

### Critical Rules

#### ❌ Rule 1: Layouts Cannot Be Nested

```scala
// ❌ WRONG - Nested layouts
case class FluidState(...) extends Layout
case class FluidStateDouble(
  current: FluidState,   // ❌ ERROR!
  previous: FluidState   // ❌ ERROR!
) extends Layout
```

```scala
// ✅ CORRECT - Flattened layout
case class FluidStateDouble(
  velocityCurrent: GBuffer[Vec3[Float32]],
  velocityPrevious: GBuffer[Vec3[Float32]],
  pressureCurrent: GBuffer[Float32],
  pressurePrevious: GBuffer[Float32],
  params: GUniform[FluidParams]
) extends Layout
```

**Why?** Cyfra's layout system needs direct access to all bindings for descriptor set creation.

#### ❌ Rule 2: No GBuffer/GUniform Constructor Calls in Layout Definition

```scala
// ❌ WRONG - Trying to construct in case class
case class MyLayout(
  data: GBuffer[Float32] = GBuffer[Float32](1024)  // ❌ ERROR!
) extends Layout
```

```scala
// ✅ CORRECT - Just type declarations
case class MyLayout(
  data: GBuffer[Float32]  // ✅ Type only
) extends Layout
```

**Why?** Layouts are type definitions, not runtime values. Construction happens in the layout function.

### Layout Function Pattern

```scala
val program = GProgram[Params, MyLayout](
  layout = params => MyLayout(
    input = GBuffer[Float32](params.size),      // ✅ Construct here
    output = GBuffer[Float32](params.size),
    params = GUniform(MyParams(params.value))
  ),
  dispatch = ...,
  workgroupSize = ...
)
```

**Key Points**:
- `params` argument is the **Scala host type**, not GPU type
- Buffer sizes are regular `Int`, not `Int32`
- `GBuffer[T](size)` and `GUniform(value)` work here

---

## GIO Monad

### What is GIO?

`GIO[T]` represents **GPU I/O operations** - effectful operations like reading/writing buffers.

```scala
trait GIO[T <: Value]:
  def map[U](f: T => U): GIO[U]
  def flatMap[U](f: T => GIO[U]): GIO[U]
```

### Pure vs Effectful Operations

#### Pure Operations (Return values directly)

```scala
// Buffer reads are PURE
val value: Float32 = buffer.read(idx)
val vel: Vec3[Float32] = velocityBuffer.read(idx)

// Uniform reads are PURE
val params: MyParams = paramsUniform.read

// Math operations are PURE
val result = value * 2.0f + 1.0f

// Helper functions are PURE
val interpolated = trilinearInterpolate(buffer, pos, size)
```

#### Effectful Operations (Return GIO)

```scala
// Buffer writes are EFFECTFUL
buffer.write(idx, value): GIO[Empty]
GIO.write(buffer, idx, value): GIO[Empty]

// Printf is EFFECTFUL
GIO.printf("Value: %f", value): GIO[Empty]

// Repeat is EFFECTFUL
GIO.repeat(n)(i => ...): GIO[Empty]

// Conditional execution is EFFECTFUL
GIO.when(condition)(action): GIO[Empty]
```

### Correct For-Comprehension Patterns

#### ❌ WRONG: Using `<-` for pure reads

```scala
// ❌ ERROR: GIO.read returns T, not GIO[T]
for
  vel <- GIO.read(buffer, idx)      // ❌ Wrong!
  temp <- buffer.read(idx)           // ❌ Wrong!
  result = vel + temp
  _ <- buffer.write(idx, result)
yield Empty()
```

#### ✅ CORRECT: `val =` for pure, `<-` for effectful

```scala
// ✅ Pure reads with val
val vel = GIO.read(buffer, idx)
val temp = buffer.read(idx)
val result = vel + temp

// ✅ Only effectful write uses GIO
GIO.write(buffer, idx, result)
```

#### ✅ CORRECT: For-comprehension for multiple writes

```scala
for
  _ <- buffer1.write(idx, value1)    // ✅ GIO operation
  _ <- buffer2.write(idx, value2)    // ✅ GIO operation
  _ <- buffer3.write(idx, value3)    // ✅ GIO operation
yield Empty()
```

### GIO.when Pattern

```scala
// Conditional execution
GIO.when(idx < bufferSize):
  val value = buffer.read(idx)
  val newValue = compute(value)
  buffer.write(idx, newValue)
```

**Note**: `GIO.when` compiles to a loop that runs 0 or 1 times, not actual branching.

---

## GProgram Structure

### Complete Program Template

```scala
case class MyParams(size: Int, scale: Float)

case class MyParamsGpu(scale: Float32) extends GStruct[MyParamsGpu]

case class MyLayout(
  input: GBuffer[Float32],
  output: GBuffer[Float32],
  params: GUniform[MyParamsGpu]
) extends Layout

val program = GProgram[MyParams, MyLayout](
  // Layout function: Scala params → Layout
  layout = params => MyLayout(
    input = GBuffer[Float32](params.size),
    output = GBuffer[Float32](params.size),
    params = GUniform(MyParamsGpu(params.scale))
  ),
  
  // Dispatch: Calculate workgroup count
  dispatch = (_, params) => {
    val totalThreads = params.size
    val workgroupSize = 256
    val numWorkgroups = (totalThreads + workgroupSize - 1) / workgroupSize
    GProgram.StaticDispatch((numWorkgroups, 1, 1))
  },
  
  // Workgroup size: (x, y, z)
  workgroupSize = (256, 1, 1)
  
// GPU shader body
): layout =>
  val idx = GIO.invocationId
  val params = layout.params.read
  
  GIO.when(idx < params.size):
    val inputValue = layout.input.read(idx)
    val result = inputValue * params.scale
    layout.output.write(idx, result)
```

### Dispatch Calculation

**Purpose**: Calculate how many workgroups are needed for N threads.

```scala
dispatch = (_, params) => {
  val totalThreads = params.size  // Regular Scala Int
  val workgroupSize = 256          // Threads per workgroup
  
  // Ceiling division: (total + size - 1) / size
  val numWorkgroups = (totalThreads + workgroupSize - 1) / workgroupSize
  
  GProgram.StaticDispatch((numWorkgroups, 1, 1))
}
```

**Common Mistake**: Using GPU types (Int32) instead of Scala types (Int).

**Correct Types**:
- In `dispatch`: Use regular Scala `Int`
- In shader body: Use GPU `Int32`

---

## Buffer Operations

### Reading Buffers

```scala
// Direct read (pure operation)
val value: Float32 = buffer.read(idx)

// Via GIO.read (also pure)
val value: Float32 = GIO.read(buffer, idx)

// These are equivalent and return T directly, not GIO[T]
```

### Writing Buffers

```scala
// Method syntax
buffer.write(idx, value): GIO[Empty]

// Function syntax
GIO.write(buffer, idx, value): GIO[Empty]

// Both return GIO[Empty], must be used in GIO context
```

### Bounds Checking Pattern

```scala
val idx = GIO.invocationId

GIO.when(idx < bufferSize):
  val value = buffer.read(idx)
  val result = compute(value)
  buffer.write(idx, result)
```

**Why?** Dispatch may launch more threads than needed (due to workgroup alignment).

---

## Helper Functions

### Math Functions

#### Float32 Functions

```scala
// Available in DSL
import io.computenode.cyfra.dsl.library.Functions.*

// Trigonometry
sin(x), cos(x), tan(x)
asin(x), acos(x), atan(x)

// Exponential
pow(x, y), exp(x), log(x), sqrt(x)

// Common
abs(x), floor(x), ceil(x), round(x)
clamp(x, minVal, maxVal)
mix(a, b, t)  // Linear interpolation

// Vector functions
min(v1, v2)  // Component-wise for Vec2/Vec3/Vec4
max(v1, v2)  // Component-wise for Vec2/Vec3/Vec4

// Float32 only
min(f1, f2)  // For Float32
max(f1, f2)  // For Float32
```

#### ❌ Int32 Functions Don't Exist

```scala
// ❌ ERROR: min/max only exist for Float32 and vectors
val x1: Int32 = min(x0 + 1, size - 1)  // ❌ No overload!
```

```scala
// ✅ SOLUTION 1: Implement helper
inline def minInt32(a: Int32, b: Int32)(using Source): Int32 =
  when(a < b)(a).otherwise(b)

val x1 = minInt32(x0 + 1, size - 1)  // ✅ Works
```

```scala
// ✅ SOLUTION 2: Use when/otherwise directly
val x1 = when(x0 + 1 < size - 1):
  x0 + 1
.otherwise:
  size - 1
```

### Int32 Operators

**Problem**: Int32 arithmetic needs imports.

```scala
// ❌ ERROR: * is not a member of Int32
val total = n * n * n
```

```scala
// ✅ SOLUTION: Import DSL or algebra
import io.computenode.cyfra.dsl.{*, given}

val total = n * n * n  // ✅ Works
```

**Available Operators** (with imports):
- Arithmetic: `+`, `-`, `*`, `/`
- Modulo: `.mod(n)` (not `%`)
- Comparison: `<`, `>`, `<=`, `>=`, `===`, `!==`

### Custom Helper Functions

```scala
/** 3D index to 1D flattened index */
inline def idx3D(x: Int32, y: Int32, z: Int32, n: Int32): Int32 =
  x + y * n + z * n * n

/** Bounds checking */
inline def inBounds(x: Int32, y: Int32, z: Int32, n: Int32): GBoolean =
  (x >= 0) && (x < n) && (y >= 0) && (y < n) && (z >= 0) && (z < n)

/** Safe buffer read with bounds check */
def readSafe(buffer: GBuffer[Float32], x: Int32, y: Int32, z: Int32, n: Int32)
            (using Source): Float32 =
  when(inBounds(x, y, z, n)):
    buffer.read(idx3D(x, y, z, n))
  .otherwise:
    0.0f
```

**Key Point**: These compile to GPU code with no overhead (inline).

---

## Best Practices

### 1. Type System

✅ **DO**: Use GPU types everywhere in shader code
```scala
val x: Float32 = 1.0f
val i: Int32 = 42
```

❌ **DON'T**: Mix host and device types
```scala
val x: Float = 1.0f  // ❌ Host type in GPU code
```

### 2. Control Flow

✅ **DO**: Use `when/otherwise`
```scala
val result = when(x > 0f):
  x * 2f
.otherwise:
  0f
```

❌ **DON'T**: Use `if/else`
```scala
val result = if (x > 0f) x * 2f else 0f  // ❌ Not GPU code
```

### 3. Equality

✅ **DO**: Use `===` and `!==`
```scala
when(x === 0f):
  // ...
```

❌ **DON'T**: Use `==` and `!=`
```scala
when(x == 0f):  // ❌ Scala equality, not GPU
```

### 4. GIO Operations

✅ **DO**: Use `val` for pure reads
```scala
val value = buffer.read(idx)
```

❌ **DON'T**: Use `<-` for pure operations
```scala
for value <- buffer.read(idx) do ...  // ❌ Wrong!
```

### 5. Layout Design

✅ **DO**: Flatten all bindings
```scala
case class Layout(
  buffer1: GBuffer[Float32],
  buffer2: GBuffer[Float32]
) extends Layout
```

❌ **DON'T**: Nest layouts
```scala
case class Layout(
  state: OtherLayout  // ❌ Cannot nest!
) extends Layout
```

### 6. Bounds Checking

✅ **DO**: Always check bounds
```scala
GIO.when(idx < bufferSize):
  buffer.write(idx, value)
```

❌ **DON'T**: Assume exact thread count
```scala
buffer.write(idx, value)  // ❌ May be out of bounds!
```

### 7. Imports

✅ **DO**: Import DSL at top
```scala
import io.computenode.cyfra.dsl.{*, given}
```

This provides:
- Int32/Float32 operators
- Vector constructors
- Conversion functions
- Control flow

---

## Common Patterns

### Pattern 1: Grid Operation

```scala
val program = GProgram[Int, Layout](
  layout = size => Layout(data = GBuffer[Float32](size)),
  dispatch = (_, size) => 
    GProgram.StaticDispatch(((size + 255) / 256, 1, 1)),
  workgroupSize = (256, 1, 1)
): layout =>
  val idx = GIO.invocationId
  GIO.when(idx < size):
    val value = layout.data.read(idx)
    val result = compute(value)
    layout.data.write(idx, result)
```

### Pattern 2: 3D Grid Operation

```scala
GIO.when(idx < totalCells):
  // Convert 1D to 3D
  val z = idx / (n * n)
  val y = (idx / n).mod(n)
  val x = idx.mod(n)
  
  // Compute
  val result = compute(x, y, z)
  
  // Write
  buffer.write(idx, result)
```

### Pattern 3: Double-Buffered Algorithm

```scala
case class DoubleBufferedLayout(
  current: GBuffer[Float32],
  previous: GBuffer[Float32]
) extends Layout

// Ping-pong: Read from previous, write to current
val value = layout.previous.read(idx)
val neighbors = readNeighbors(layout.previous, x, y, z)
val newValue = compute(value, neighbors)
layout.current.write(idx, newValue)

// Then swap current ↔ previous for next iteration
```

### Pattern 4: Trilinear Interpolation

```scala
def trilinearInterpolate(
  buffer: GBuffer[Vec3[Float32]], 
  pos: Vec3[Float32], 
  size: Int32
)(using Source): Vec3[Float32] =
  val x = clamp(pos.x, 0.0f, (size - 1).asFloat)
  val y = clamp(pos.y, 0.0f, (size - 1).asFloat)
  val z = clamp(pos.z, 0.0f, (size - 1).asFloat)
  
  val x0 = x.asInt
  val y0 = y.asInt
  val z0 = z.asInt
  val x1 = minInt32(x0 + 1, size - 1)
  val y1 = minInt32(y0 + 1, size - 1)
  val z1 = minInt32(z0 + 1, size - 1)
  
  val fx = x - x0.asFloat
  val fy = y - y0.asFloat
  val fz = z - z0.asFloat
  
  // Sample 8 corners
  val v000 = buffer.read(idx3D(x0, y0, z0, size))
  val v100 = buffer.read(idx3D(x1, y0, z0, size))
  // ... (6 more samples)
  
  // Trilinear blend
  val v0 = mix(mix(v000, v100, fx), mix(v010, v110, fx), fy)
  val v1 = mix(mix(v001, v101, fx), mix(v011, v111, fx), fy)
  mix(v0, v1, fz)
```

---

## Runtime Management

### Creating Runtime

```scala
// Keep concrete type for close() method
val runtime = VkCyfraRuntime()
given CyfraRuntime = runtime

try
  // Use runtime
  program.run(params)
finally
  runtime.close()  // ✅ Cleanup
```

**Why concrete type?** `CyfraRuntime` trait doesn't have `close()`, only `VkCyfraRuntime` does.

### Shader Caching

```scala
// Shaders are cached by hash automatically
val runtime = VkCyfraRuntime()

// First run: Compiles shader
program.run(params)

// Second run: Uses cached shader
program.run(params)  // ✅ Fast!
```

**Cache key**: SPIR-V bytecode hash (program logic)

---

## Debugging

### Printf Debugging

```scala
GIO.when(idx === 0):  // Print from first thread only
  GIO.printf("Value at 0: %f", buffer.read(0))
```

### SPIR-V Disassembly

```scala
val runtime = VkCyfraRuntime(
  spirvToolsRunner = SpirvToolsRunner(
    disassembler = SpirvDisassembler.Enable(
      toolOutput = SpirvTool.ToFile("shader.spvasm")
    )
  )
)
```

### Validation Layers

```scala
// Enable for detailed Vulkan errors
// Requires Vulkan SDK
-Dio.computenode.cyfra.vulkan.validation=true
```

---

## Summary Checklist

- [ ] Use GPU types (`Float32`, `Int32`, `Vec3`, etc.)
- [ ] Import DSL: `import io.computenode.cyfra.dsl.{*, given}`
- [ ] Flatten layouts (no nesting)
- [ ] Use `val` for pure reads, `<-` for GIO operations
- [ ] Check bounds before buffer access
- [ ] Use `when/otherwise` for conditionals
- [ ] Use `===` for equality checks
- [ ] Implement `minInt32`/`maxInt32` if needed
- [ ] Close runtime in `finally` block
- [ ] Keep dispatch calculations in host types (Int, not Int32)

