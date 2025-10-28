# Cyfra Troubleshooting Guide

## Table of Contents
1. [Compilation Errors](#compilation-errors)
2. [Type System Issues](#type-system-issues)
3. [Layout Definition Problems](#layout-definition-problems)
4. [GIO Operation Errors](#gio-operation-errors)
5. [Runtime Errors](#runtime-errors)
6. [Performance Issues](#performance-issues)

---

## Compilation Errors

### Error 1: "object GBuffer does not take type parameters"

**Symptoms**:
```
Error: object GBuffer in package io.computenode.cyfra.dsl.binding does not take type parameters
Location: velocity = GBuffer[Vec3[Float32]](totalCells)
```

**Root Cause**:
Trying to call `GBuffer[T](size)` in a case class definition instead of in the layout function.

**Wrong Pattern**:
```scala
case class MyLayout(
  data: GBuffer[Float32] = GBuffer[Float32](1024)  // ❌ ERROR!
) extends Layout
```

**Solution**:
```scala
// 1. Case class: just type declaration
case class MyLayout(
  data: GBuffer[Float32]  // ✅ No construction
) extends Layout

// 2. Construction happens in layout function
val program = GProgram[Int, MyLayout](
  layout = size => MyLayout(
    data = GBuffer[Float32](size)  // ✅ Construct here
  ),
  ...
)
```

**Why it works**: Layout case classes are type definitions. Actual GBuffer instances are created by the runtime based on the layout function.

---

### Error 2: "'<-' expected, but '=' found" in for-comprehension

**Symptoms**:
```
Error: '<-' expected, but '=' found
Location: for { velXP = readVec3Safe(...) }
```

**Root Cause**:
Mixing syntax styles in for-comprehension. Using `=` for binding instead of `val`.

**Wrong Pattern**:
```scala
for
  velXP = readVec3Safe(buffer, x + 1, y, z, n)  // ❌ Ambiguous
  result = compute(velXP)
  _ <- write(buffer, idx, result)
yield Empty()
```

**Solution Option 1**: Use `val` outside for-comprehension
```scala
// Pure operations
val velXP = readVec3Safe(buffer, x + 1, y, z, n)
val velXM = readVec3Safe(buffer, x - 1, y, z, n)
val result = compute(velXP, velXM)

// Only effectful operation
write(buffer, idx, result)
```

**Solution Option 2**: Pure assignment with `GIO.pure`
```scala
for
  _ <- GIO.pure(())  // Start GIO context
  velXP = readVec3Safe(buffer, x + 1, y, z, n)  // Now = is OK
  result = compute(velXP)
  _ <- write(buffer, idx, result)
yield Empty()
```

**Recommendation**: Option 1 is clearer - keep pure and effectful code separate.

---

### Error 3: "value map is not a member of Vec3[Float32]"

**Symptoms**:
```
Error: value map/flatMap is not a member of Vec3[Float32]
Location: for { vel <- GIO.read(state.velocity, idx) }
```

**Root Cause**:
Treating pure read operations as if they return `GIO[T]`. They return `T` directly.

**Wrong Pattern**:
```scala
for
  vel <- GIO.read(state.velocity, idx)  // ❌ Returns Vec3, not GIO[Vec3]!
  temp <- buffer.read(idx)               // ❌ Same problem
  result = vel + temp
  _ <- write(buffer, idx, result)
yield Empty()
```

**Solution**:
```scala
// Reads are pure - use val, not <-
val vel = GIO.read(state.velocity, idx)
val temp = buffer.read(idx)
val result = vel + temp

// Only write needs GIO
write(buffer, idx, result)
```

**Key Understanding**:
- `buffer.read(idx)` → Returns `T` (pure)
- `GIO.read(buffer, idx)` → Returns `T` (pure, equivalent to above)
- `buffer.write(idx, value)` → Returns `GIO[Empty]` (effectful)

**Memory Aid**: If it's reading, it's pure. If it's writing, it's GIO.

---

### Error 4: "value * is not a member of Int32"

**Symptoms**:
```
Error: value * is not a member of io.computenode.cyfra.dsl.Int32
Location: val total = gridSize * gridSize * gridSize
```

**Root Cause**:
Int32 operators require DSL imports to work.

**Wrong Pattern**:
```scala
package mypackage

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.dsl.Value.Int32

val total = n * n * n  // ❌ * not found
```

**Solution**:
```scala
package mypackage

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.dsl.{*, given}  // ✅ Import DSL

val total = n * n * n  // ✅ Works
```

**What the wildcard import provides**:
- Arithmetic operators: `+`, `-`, `*`, `/`
- Comparison operators: `<`, `>`, `<=`, `>=`
- Type conversions: `.asFloat`, `.asInt`, `.unsigned`
- Vector constructors: `vec2`, `vec3`, `vec4`

---

### Error 5: "None of the overloaded alternatives of method min match arguments (Int32, Int32)"

**Symptoms**:
```
Error: None of the overloaded alternatives of method min with types
  (Float32, Float32) → Float32
  (Vec[Float32], Vec[Float32]) → Vec[Float32]
match arguments (Int32, Int32)
```

**Root Cause**:
`Functions.min()` and `Functions.max()` only have overloads for `Float32` and vector types, NOT for `Int32`.

**Wrong Pattern**:
```scala
import io.computenode.cyfra.dsl.library.Functions.min

val x1: Int32 = min(x0 + 1, size - 1)  // ❌ No Int32 overload!
```

**Solution 1**: Implement helper function
```scala
object GridUtils:
  inline def minInt32(a: Int32, b: Int32)(using Source): Int32 =
    when(a < b)(a).otherwise(b)
  
  inline def maxInt32(a: Int32, b: Int32)(using Source): Int32 =
    when(a > b)(a).otherwise(b)

// Usage
val x1 = minInt32(x0 + 1, size - 1)  // ✅ Works
```

**Solution 2**: Use when/otherwise directly
```scala
val x1 = when(x0 + 1 < size - 1):
  x0 + 1
.otherwise:
  size - 1
```

**Why not convert to Float32?**
- Extra operations (asFloat, asInt)
- Potential precision loss
- Less clear intent
- Helper function is cleaner

---

### Error 6: "is not a GBinding, all elements of a Layout must be GBindings"

**Symptoms**:
```
Error: A literal string is expected as an argument to `compiletime.error`. Got ...
"[name] is not a GBinding, all elements of a Layout must be GBindings"
```

**Root Cause**:
Trying to nest layouts - having a field in a Layout that is itself a Layout.

**Wrong Pattern**:
```scala
case class FluidState(...) extends Layout

case class FluidStateDouble(
  current: FluidState,    // ❌ ERROR: Layout as field!
  previous: FluidState    // ❌ ERROR: Layout as field!
) extends Layout
```

**Solution**: Flatten all buffers to single level
```scala
case class FluidStateDouble(
  // Current state buffers
  velocityCurrent: GBuffer[Vec3[Float32]],
  pressureCurrent: GBuffer[Float32],
  densityCurrent: GBuffer[Float32],
  
  // Previous state buffers
  velocityPrevious: GBuffer[Vec3[Float32]],
  pressurePrevious: GBuffer[Float32],
  densityPrevious: GBuffer[Float32],
  
  // Shared params
  params: GUniform[FluidParams]
) extends Layout
```

**Why?**
- Cyfra's layout system needs direct access to all GBuffer/GUniform fields
- Nested structures break descriptor set creation
- All bindings must be at top level for Vulkan

**Design Pattern**: Use naming convention to indicate grouping:
- `*Current` / `*Previous` for ping-pong buffers
- `*Input` / `*Output` for data flow
- `*Read` / `*Write` for separate access

---

### Error 7: "value execute is not a member of GExecution"

**Symptoms**:
```
Error: value execute is not a member of io.computenode.cyfra.core.GExecution
Location: execution.execute(params, state)
```

**Root Cause**:
Using incorrect method name on GExecution API.

**Wrong Pattern**:
```scala
val execution = GExecution[Params, Layout]()
  .addProgram(program1)(...)
  .addProgram(program2)(...)

val result = execution.execute(params, layout)  // ❌ Wrong method name
```

**Solution**: Check API documentation for correct method
```scala
// Need to verify actual method name - possibilities:
// execution.run(params, layout)
// execution.apply(params, layout)
// Or different signature entirely
```

**How to investigate**:
1. Search codebase for `GExecution` usage in examples
2. Check `GExecution.scala` source for method definitions
3. Look at test files for usage patterns

**Temporary workaround**:
```scala
def step(state: State): State =
  // TODO: Implement when GExecution API is verified
  state
```

---

### Error 8: "value close is not a member of CyfraRuntime"

**Symptoms**:
```
Error: value close is not a member of io.computenode.cyfra.core.CyfraRuntime
Location: runtime.close()
```

**Root Cause**:
`close()` method is on `VkCyfraRuntime` class, not on `CyfraRuntime` trait.

**Wrong Pattern**:
```scala
given CyfraRuntime = VkCyfraRuntime()
val runtime = summon[CyfraRuntime]

try
  // ...
finally
  runtime.close()  // ❌ CyfraRuntime trait has no close()
```

**Solution**: Keep concrete type
```scala
val runtime = VkCyfraRuntime()  // ✅ Concrete type
given CyfraRuntime = runtime     // For implicit

try
  // ...
finally
  runtime.close()  // ✅ Works - VkCyfraRuntime has close()
```

**Alternative**: Type annotation
```scala
given runtime: VkCyfraRuntime = VkCyfraRuntime()

try
  // ...
finally
  runtime.close()
```

---

## Type System Issues

### Issue 1: Mixing Host and Device Types

**Problem**:
```scala
val scalaInt: Int = 42
val gpuInt: Int32 = 42

val result = scalaInt + gpuInt  // ❌ Type mismatch
```

**Solution**: Convert host values to device types
```scala
val scalaInt: Int = 42
val gpuInt: Int32 = scalaInt  // ✅ Implicit conversion

// Or explicit in layout params
case class Params(value: Int)  // Host param
case class ParamsGpu(value: Int32) extends GStruct[ParamsGpu]

layout = params => Layout(
  params = GUniform(ParamsGpu(params.value))  // Int → Int32 conversion
)
```

**Rule**: 
- **Layout function**: Host types (Scala `Int`, `Float`)
- **Shader body**: Device types (`Int32`, `Float32`)

---

### Issue 2: Boolean Comparisons

**Problem**:
```scala
when(x == 0f):  // ❌ Using Scala equality
  // ...
```

**Solution**: Use DSL equality operators
```scala
when(x === 0f):  // ✅ GPU equality
  // ...

when(x !== 0f):  // ✅ GPU inequality
  // ...
```

**Available GPU comparisons**:
- `<`, `>`, `<=`, `>=` (same as Scala)
- `===` (GPU equals)
- `!==` (GPU not equals)

---

### Issue 3: Modulo Operator

**Problem**:
```scala
val remainder = idx % n  // ❌ % not defined for Int32
```

**Solution**: Use `.mod()` method
```scala
val remainder = idx.mod(n)  // ✅ Correct
```

**Why?** GLSL uses `mod()` function, not `%` operator.

---

## Layout Definition Problems

### Problem 1: Buffer Size Mismatches

**Scenario**: Passing wrong type for buffer size in layout function.

**Wrong**:
```scala
case class Params(gridSize: Int32)  // ❌ GPU type as param

layout = params => Layout(
  buffer = GBuffer[Float32](params.gridSize)  // ❌ Expects Int, got Int32
)
```

**Correct**:
```scala
case class Params(gridSize: Int)  // ✅ Host type

layout = params => Layout(
  buffer = GBuffer[Float32](params.gridSize)  // ✅ Works
)
```

**Rule**: Parameters in layout function are **host types** (Scala Int, not Int32).

---

### Problem 2: Dispatch Calculation Confusion

**Scenario**: Using GPU types in dispatch function.

**Wrong**:
```scala
dispatch = (_, params) => {
  val n = params.gridSize  // Int32 from GPU struct
  val total = n * n * n    // Int32 arithmetic
  StaticDispatch(((total + 255) / 256, 1, 1))  // ❌ Type error
}
```

**Correct**:
```scala
dispatch = (_, params) => {
  // params is the Scala host type, not GPU type
  val n = params.gridSize  // Regular Scala Int
  val total = n * n * n    // Regular arithmetic
  val workgroups = (total + 255) / 256
  StaticDispatch((workgroups, 1, 1))  // ✅ Works
}
```

**Key Insight**: In `dispatch`, `params` is the **Scala host type**, NOT the GPU struct type.

---

### Problem 3: GUniform.fromParams Confusion

**Scenario**: Seeing `GUniform.fromParams` in examples and not understanding it.

**Example**:
```scala
case class Layout(
  params: GUniform[MyParams] = GUniform.fromParams
) extends Layout
```

**What it means**: This is a marker telling the runtime to automatically populate from program parameters.

**When to use**:
- If your GUniform exactly matches your program parameters
- Automatic marshalling from host to device types

**When NOT to use**:
- If you need to transform parameters
- If GPU and host param structures differ

**Manual approach**:
```scala
layout = params => Layout(
  params = GUniform(MyParamsGpu(
    value = params.hostValue,
    scale = params.hostScale
  ))
)
```

---

## GIO Operation Errors

### Problem 1: Forgetting Bounds Check

**Scenario**: Writing to buffer without checking thread index.

**Wrong**:
```scala
): layout =>
  val idx = GIO.invocationId
  val value = layout.input.read(idx)   // ❌ May be out of bounds
  layout.output.write(idx, value * 2)  // ❌ May be out of bounds
```

**Correct**:
```scala
): layout =>
  val idx = GIO.invocationId
  
  GIO.when(idx < bufferSize):  // ✅ Bounds check
    val value = layout.input.read(idx)
    layout.output.write(idx, value * 2)
```

**Why**: Workgroup dispatch rounds up, so extra threads may be launched.

**Example**: 
- Buffer size: 1000
- Workgroup size: 256
- Workgroups needed: `(1000 + 255) / 256 = 4`
- Threads launched: `4 * 256 = 1024`
- Extra threads: `1024 - 1000 = 24` (need bounds check!)

---

### Problem 2: Incorrect GIO Composition

**Scenario**: Trying to compose GIO operations incorrectly.

**Wrong**:
```scala
val result = buffer.write(idx, value)  // Returns GIO[Empty]
doSomethingWith(result)  // ❌ Can't use GIO[Empty] as value
```

**Correct**: Use for-comprehension or flatMap
```scala
// Sequential writes
for
  _ <- buffer1.write(idx, value1)
  _ <- buffer2.write(idx, value2)
yield Empty()

// Or flatMap
buffer1.write(idx, value1).flatMap: _ =>
  buffer2.write(idx, value2)
```

---

### Problem 3: Trying to Return Values from GIO

**Scenario**: Expecting GIO operations to return computed values.

**Wrong**:
```scala
def compute(idx: Int32): Float32 =
  val value = buffer.read(idx)
  val result = value * 2
  buffer.write(idx, result)  // ❌ Returns GIO[Empty], not Float32
```

**Correct**: Separate pure and effectful
```scala
def computePure(value: Float32): Float32 =
  value * 2

def computeWithIO(idx: Int32): GIO[Empty] =
  val value = buffer.read(idx)
  val result = computePure(value)
  buffer.write(idx, result)
```

---

## Runtime Errors

### Error 1: Vulkan Validation Errors

**Symptoms**: Crashes with Vulkan validation layer errors at runtime.

**Causes**:
1. Out-of-bounds buffer access
2. Incorrect descriptor set layout
3. Memory alignment issues
4. Invalid SPIR-V code

**Debug Strategy**:
1. Enable validation layers:
   ```
   -Dio.computenode.cyfra.vulkan.validation=true
   ```
2. Check SPIR-V disassembly:
   ```scala
   val runtime = VkCyfraRuntime(
     spirvToolsRunner = SpirvToolsRunner(
       disassembler = SpirvDisassembler.Enable(...)
     )
   )
   ```
3. Add bounds checks in shader
4. Use GIO.printf to debug values

---

### Error 2: GPU Hanging

**Symptoms**: Program freezes, GPU becomes unresponsive.

**Causes**:
1. Infinite loops in shader
2. Very large dispatch
3. Too many threads

**Solutions**:
1. Add iteration limits to GSeq:
   ```scala
   GSeq.gen(...).limit(maxIterations)  // ✅ Prevent infinite loops
   ```
2. Validate dispatch calculations
3. Test with small data first

---

### Error 3: Incorrect Results

**Symptoms**: Program runs but produces wrong output.

**Debug Steps**:

1. **Add printf debugging**:
   ```scala
   GIO.when(idx === 0):  // First thread only
     GIO.printf("Input[0]: %f", buffer.read(0))
     GIO.printf("Output[0]: %f", outBuffer.read(0))
   ```

2. **Test with simple data**:
   ```scala
   // Fill with known pattern
   val testData = Array.fill(100)(1.0f)
   // Expected output?
   ```

3. **Check algorithm step-by-step**:
   - Verify each buffer separately
   - Test pure functions in Scala first
   - Compare with reference implementation

4. **Verify buffer sizes match**:
   ```scala
   assert(inputSize == outputSize)
   ```

---

## Performance Issues

### Issue 1: Slow Compilation

**Cause**: First run compiles shaders, which is slow.

**Solution**: Shaders are cached automatically on subsequent runs.

**Verify caching**:
```scala
// First run: ~5 seconds
program.run(params)

// Second run: ~0.1 seconds (cached)
program.run(params)
```

---

### Issue 2: Slow Execution

**Causes**:
1. Too many Jacobi iterations
2. Inefficient memory access patterns
3. Small workgroup sizes
4. Memory bandwidth bound

**Solutions**:
1. **Reduce iteration count**:
   ```scala
   // Trade accuracy for speed
   val iterations = 20  // Down from 40
   ```

2. **Optimize workgroup size**:
   ```scala
   // Try different sizes
   workgroupSize = (256, 1, 1)  // vs (64, 1, 1) vs (512, 1, 1)
   ```

3. **Batch operations**:
   ```scala
   // Process multiple elements per thread
   GIO.repeat(elementsPerThread): offset =>
     val idx = GIO.invocationId * elementsPerThread + offset
     // ...
   ```

4. **Profile with smaller grids first**:
   ```scala
   // Start small
   val gridSize = 32  // 32³ = 32K cells
   // Scale up: 64³ = 256K cells, 128³ = 2M cells
   ```

---

## Quick Reference: Error → Solution

| Error Message | Quick Fix |
|---------------|-----------|
| "GBuffer does not take type parameters" | Move construction to layout function |
| "'<-' expected, but '=' found" | Use `val` outside for-comprehension |
| "value map is not a member of" | Don't use `<-` for reads, use `val` |
| "value * is not a member of Int32" | Add `import io.computenode.cyfra.dsl.{*, given}` |
| "method min match arguments Int32" | Implement `minInt32` helper |
| "not a GBinding" | Flatten nested layouts |
| "value execute is not a member" | Check GExecution API, might be `.run()` |
| "value close is not a member" | Use concrete `VkCyfraRuntime` type |

---

## Getting Help

1. **Check examples**: `cyfra-examples/src/main/scala/`
2. **Read source**: Most issues clarified by reading API source
3. **Enable validation**: Catches errors early
4. **Use printf**: Debug values at runtime
5. **Start simple**: Test with minimal programs first

---

## Summary: Development Workflow

1. ✅ Start with working example
2. ✅ Modify incrementally
3. ✅ Compile after each change
4. ✅ Test with small data
5. ✅ Add printf debugging
6. ✅ Scale up gradually
7. ✅ Profile and optimize

**Remember**: GPU debugging is harder than CPU. Invest time in getting compilation right before runtime testing.

