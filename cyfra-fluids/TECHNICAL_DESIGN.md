# 3D Navier-Stokes Fluid Simulation - Technical Design Document

## Executive Summary

This document details the technical approach for implementing a GPU-accelerated 3D incompressible fluid simulator using Cyfra's DSL and compute capabilities. The implementation follows the **Stable Fluids** method by Jos Stam, which provides unconditionally stable simulation through operator splitting.

## Why This Approach Works for Cyfra

### Cyfra's Strengths Leveraged

1. **Compute-Focused**: Fluid simulation is compute-intensive, perfect for Cyfra's SPIR-V/Vulkan backend
2. **GExecution Chaining**: Natural fit for multi-pass algorithms (advection → diffusion → projection)
3. **Double Buffering**: Layouts support ping-pong between current/previous state
4. **Type-Safe DSL**: Prevents common GPU programming errors (type mismatches, index errors)
5. **GStruct for Parameters**: Clean parameter passing without manual buffer management
6. **Foton Integration**: Existing animation infrastructure for time-series output

### Design Constraints

1. **No Sparse Structures**: Cyfra uses dense GBuffer arrays, so we use uniform grids (not octrees)
2. **No Atomics**: No built-in atomic operations, so we avoid parallel reductions
3. **No Shared Memory Access**: Current DSL doesn't expose workgroup shared memory (yet)
4. **Fixed-Size Buffers**: Grid size determined at layout creation time

## Grid Representation Deep Dive

### 3D → 1D Indexing

We store the 3D grid `(x, y, z)` in a flattened 1D array using **row-major order**:

```
index = x + y * width + z * width * height
```

For a cube grid where `width = height = depth = N`:

```
index = x + y * N + z * N²
```

This pattern is efficient for GPU memory access and matches typical texture layouts.

### Memory Layout Example

For a 4³ grid (64 cells):

```
Cell (0,0,0) → index 0
Cell (1,0,0) → index 1
Cell (2,0,0) → index 2
Cell (3,0,0) → index 3
Cell (0,1,0) → index 4
...
Cell (3,3,3) → index 63
```

### Neighbor Access Pattern

Each cell has 6 face neighbors (±x, ±y, ±z):

```scala
def getNeighbors6(
  buffer: GBuffer[Vec3[Float32]],
  x: Int32, y: Int32, z: Int32,
  n: Int32
)(using Source): (Vec3[Float32], Vec3[Float32], Vec3[Float32], 
                   Vec3[Float32], Vec3[Float32], Vec3[Float32]) =
  
  val xm = when(x > 0)(buffer.read(idx3D(x - 1, y, z, n))).otherwise(vec3(0f, 0f, 0f))
  val xp = when(x < n - 1)(buffer.read(idx3D(x + 1, y, z, n))).otherwise(vec3(0f, 0f, 0f))
  val ym = when(y > 0)(buffer.read(idx3D(x, y - 1, z, n))).otherwise(vec3(0f, 0f, 0f))
  val yp = when(y < n - 1)(buffer.read(idx3D(x, y + 1, z, n))).otherwise(vec3(0f, 0f, 0f))
  val zm = when(z > 0)(buffer.read(idx3D(x, y, z - 1, n))).otherwise(vec3(0f, 0f, 0f))
  val zp = when(z < n - 1)(buffer.read(idx3D(x, y, z + 1, n))).otherwise(vec3(0f, 0f, 0f))
  
  (xm, xp, ym, yp, zm, zp)
```

## Operator Splitting Explained

The Navier-Stokes equations are complex and coupled. Operator splitting decomposes them into simpler, independent steps that can be solved sequentially.

### Original Equation

```
∂u/∂t = -(u·∇)u - ∇p/ρ + ν∇²u + f
       └─────┬───┘  └───┬──┘  └─┬─┘  └┬┘
       advection  pressure  diffusion forces
```

### Split Into Steps

Each term becomes a separate sub-problem:

1. **Forces**: `∂u/∂t = f` → Simple Euler integration
2. **Advection**: `∂u/∂t = -(u·∇)u` → Semi-Lagrangian advection
3. **Diffusion**: `∂u/∂t = ν∇²u` → Implicit solve
4. **Projection**: `∇·u = 0` → Pressure correction

Each step is **unconditionally stable**, meaning we can use large time steps without numerical explosion.

## Algorithm Details

### 1. Forces Step

**Physics**: Apply body forces (gravity, buoyancy) and external forces (user interaction).

**Buoyancy Model** (Boussinesq approximation):
```
F_buoyancy = α * (T - T_ambient) * g
```

Where:
- `α` = thermal expansion coefficient
- `T` = local temperature
- `T_ambient` = reference temperature
- `g` = gravity vector (0, -9.8, 0)

Hot fluid is less dense and rises.

**Implementation**:
```scala
val newVel = oldVel + (forces + buoyancy) * dt
```

**Complexity**: O(N³) - one pass, embarrassingly parallel

### 2. Advection Step

**Physics**: Transport quantities along the velocity field.

**Problem**: Eulerian grids are fixed, but fluid moves. How do we update `u(x, t+Δt)`?

**Solution**: Semi-Lagrangian Advection
1. For each grid point `x`, backtrace: where was the particle at time `t`?
2. Look up the velocity at that past location
3. Use that as the new velocity at `x`

**Algorithm**:
```
x_prev = x - u(x, t) * Δt   (backward integration)
u(x, t+Δt) = u(x_prev, t)   (lookup with interpolation)
```

**Why it's stable**: We're always interpolating (smoothing), never extrapolating.

**Interpolation**: Use trilinear interpolation to sample between grid points:

```
Given position p = (px, py, pz) in continuous space:
1. Find surrounding 8 grid cells (cube corners)
2. Compute fractional position within cube
3. Blend 8 values: lerp(lerp(lerp(...)))
```

**Complexity**: O(N³) - one pass per field (velocity, density, temperature)

### 3. Diffusion Step

**Physics**: Viscosity causes velocity to spread out (momentum diffusion).

**Equation**: `(I - ν·Δt·∇²)u^{n+1} = u^n`

This is a **sparse linear system**: `A·u^{n+1} = u^n`

**Why implicit?** Explicit diffusion has CFL constraint: `Δt < (Δx)²/(2ν)` (very small!)
Implicit is unconditionally stable.

**Solver**: Jacobi Iteration

Jacobi is an iterative method:
```
u^{k+1}_i = (1/α)(b_i - Σ_{j≠i} A_{ij} u^k_j)
```

For diffusion:
```
u^{k+1}(x,y,z) = (α·u^n(x,y,z) + Σ_neighbors u^k(neighbor)) / (α + 6)
```

Where `α = 1 / (ν·Δt)`

**Convergence**: Typically 10-20 iterations for visual plausibility (not exact).

**Complexity**: O(K·N³) where K = iteration count

### 4. Pressure Projection

**Goal**: Enforce incompressibility `∇·u = 0`

**Helmholtz-Hodge Decomposition**: Any vector field can be decomposed into:
```
u = u_div-free + ∇φ
```

Where `u_div-free` is divergence-free (incompressible part).

**Strategy**:
1. Compute divergence: `d = ∇·u`
2. Solve for pressure: `∇²p = d`  (Poisson equation)
3. Subtract gradient: `u_new = u - ∇p`

**Result**: `∇·u_new = ∇·u - ∇·∇p = d - ∇²p = d - d = 0` ✓

**Poisson Solver**: Again use Jacobi iteration

```
p^{k+1}(x,y,z) = (1/6)(Σ_neighbors p^k(neighbor) - d(x,y,z))
```

**Complexity**: O(K·N³) where K = 20-50 iterations for accurate incompressibility

### 5. Boundary Conditions

**No-Slip**: Fluid velocity equals boundary velocity (zero for stationary walls)
```
u(boundary) = 0
```

**Free-Slip**: Fluid can slide along boundary (zero normal component)
```
u_normal(boundary) = 0
u_tangent(boundary) = interior value
```

**Implementation**: Simply overwrite boundary cells after each step.

**Complexity**: O(N²) - only surface cells

## Double Buffering Strategy

Many operations need to read from previous state while writing to current state.

**Naive (Wrong)**:
```scala
for i <- 0 until N³ do
  state(i) = computeNew(state(i), neighbors)  // Read-after-write hazard!
```

**Correct (Ping-Pong)**:
```scala
case class FluidStateDouble(
  current: FluidState,
  previous: FluidState
)

// Read from previous, write to current
for i <- 0 until N³ do
  current(i) = computeNew(previous(i), previous(neighbors))

// Swap
(current, previous) = (previous, current)
```

In Cyfra, we use `FluidStateDouble` with explicit `current`/`previous` buffers.

## Iterative Solver Details

### Why Jacobi Instead of Gauss-Seidel?

**Gauss-Seidel**: Uses updated values immediately
```
for i in sequential_order:
  x[i] = compute(x[0..i-1]_new, x[i..n]_old)
```
- Faster convergence (fewer iterations)
- **Cannot parallelize** (sequential dependency)

**Jacobi**: Uses all old values
```
parallel for i:
  x_new[i] = compute(x_old[neighbors])
```
- Slower convergence (more iterations)
- **Fully parallel** (perfect for GPU)

For GPU, Jacobi is faster overall despite more iterations.

### Convergence Criterion

Ideally, iterate until residual < ε:
```
residual = ||A·x^k - b||
```

**In practice**: Fixed iteration count (10-40) for predictable performance.

## Visualization: Ray Marching

### Why Ray Marching?

Fluid is a **volumetric** medium (not a surface). Traditional rasterization doesn't work.

**Ray Marching**: Shoot rays from camera, sample density along ray, accumulate color.

### Algorithm

```
for each pixel (x, y):
  ray = camera.getRay(x, y)
  color = vec4(0, 0, 0, 0)
  transmittance = 1.0
  
  for step in 0 to maxSteps:
    pos = ray.origin + ray.dir * step * stepSize
    
    density = sampleVolume(pos)
    sampleColor = densityToColor(density)
    alpha = density * absorptionCoeff
    
    color += sampleColor * alpha * transmittance
    transmittance *= (1.0 - alpha)
    
    if transmittance < threshold:
      break
  
  output(x, y) = color
```

### Density to Color Mapping

Several options:

1. **Grayscale**: `color = (d, d, d)`
2. **Temperature**: Use color ramp (blue → white → red)
3. **Velocity-based**: Color by velocity magnitude (cold = slow, hot = fast)
4. **Lighting**: Add directional light with shadow accumulation

### Optimization: Early Ray Termination

If `transmittance < 0.01`, remaining samples contribute < 1% to final color. Stop early.

### Sampling Strategy

**Uniform**: Fixed step size (simple, may miss details)
**Adaptive**: Smaller steps in high-density regions (better quality, complex)

For initial implementation: uniform sampling with step size ≈ 0.5 * cell size

## Performance Analysis

### Memory Bandwidth

**Grid size**: N³ cells
**Fields**: velocity (12 bytes), pressure (4 bytes), density (4 bytes), temperature (4 bytes) = **24 bytes/cell**
**Double buffered**: 2× = **48 bytes/cell**

**Example** (N=64):
- 64³ = 262,144 cells
- Memory: 262,144 × 48 = **12.6 MB**

**Bandwidth per frame**:
- 6 passes (advection, diffusion, projection, etc.) × 2 reads + 1 write = 3× memory
- 12.6 MB × 3 = **~40 MB/frame**

At 60 FPS: **2.4 GB/s** (well within modern GPU bandwidth ~100-500 GB/s)

### Compute Complexity

**Per frame**:
- Forces: 1 pass = N³ ops
- Advection: 1 pass × 8 reads (trilinear) = 8N³ ops
- Diffusion: K iterations × 6 reads = 6KN³ ops
- Divergence: 1 pass × 6 reads = 6N³ ops
- Pressure: K iterations × 6 reads = 6KN³ ops
- Projection: 1 pass × 6 reads = 6N³ ops

**Total**: (15 + 12K)N³ operations

**Example** (N=64, K=20):
- (15 + 240) × 262,144 = **67 million ops**
- At 1 TFLOPS: **0.067 ms** (theoretical)
- Actual: ~2-5 ms (memory bound, not compute bound)

### Bottlenecks

1. **Memory bandwidth** (reading neighbors)
2. **Iteration count** (pressure solve)
3. **Ray marching** (visualization)

**Optimization priorities**:
1. Reduce iteration count (multigrid, conjugate gradient)
2. Optimize ray marching (BVH, empty space skipping)
3. Use shared memory for neighbor access

## Numerical Stability

### CFL Condition

For advection, the Courant-Friedrichs-Lewy condition:
```
CFL = |u|·Δt / Δx < 1
```

**Interpretation**: Fluid shouldn't travel more than one cell per time step.

**Semi-Lagrangian bypasses this**: Stable even for CFL > 1, but loses detail (too much interpolation smoothing).

**Recommended**: Keep CFL ≈ 0.5-1.0 for visual quality.

### Dissipation

**Problem**: Numerical methods always dissipate energy (simulation becomes too viscous over time).

**Solutions**:
1. **Vorticity Confinement**: Add artificial vorticity to counteract dissipation
2. **Higher-order advection**: BFECC, MacCormack (less dissipative)
3. **FLIP/PIC**: Hybrid particle-grid methods

For initial implementation: Accept some dissipation, add vorticity confinement later.

### Pressure Solve Accuracy

**Problem**: Jacobi converges slowly for Poisson equation (high-frequency errors).

**Effect**: If pressure solve is inaccurate, fluid will compress/expand (volume changes).

**Solutions**:
1. **More iterations**: 40-50 instead of 20
2. **Multigrid**: O(N³) solver instead of O(K·N³)
3. **Preconditioned CG**: Conjugate gradient with preconditioner

For initial implementation: Use 40 Jacobi iterations.

## Cyfra-Specific Implementation Details

### Workgroup Size Selection

**Typical**: `(256, 1, 1)` or `(64, 1, 1)`

**Rationale**:
- Modern GPUs have 32-64 threads per warp/wavefront
- 256 = 8 warps (good occupancy)
- 1D workgroup for 1D flattened grid

**Dispatch calculation**:
```scala
val totalThreads = n * n * n
val workgroupSize = 256
val numWorkgroups = (totalThreads + workgroupSize - 1) / workgroupSize

dispatch = StaticDispatch((numWorkgroups, 1, 1))
```

### GIO Pattern for Grid Operations

```scala
val program = GProgram[Params, Layout](
  layout = params => Layout(...),
  dispatch = (layout, params) => StaticDispatch(...),
  workgroupSize = (256, 1, 1)
): layout =>
  val idx = GIO.invocationId  // Thread index
  val params = layout.params.read
  val n = params.gridSize
  
  when(idx < n * n * n):  // Bounds check
    // Convert to 3D
    val z = idx / (n * n)
    val y = (idx / n) % n
    val x = idx % n
    
    // Read from buffers
    val value = layout.buffer.read(idx)
    
    // Compute
    val result = compute(value, x, y, z, n, layout)
    
    // Write result
    for _ <- layout.buffer.write(idx, result)
    yield ()
```

### Helper Functions as DSL Functions

```scala
def idx3D(x: Int32, y: Int32, z: Int32, n: Int32): Int32 =
  x + y * n + z * n * n

def inBounds(x: Int32, y: Int32, z: Int32, n: Int32): GBoolean =
  (x >= 0) && (x < n) && (y >= 0) && (y < n) && (z >= 0) && (z < n)

def readSafe(buffer: GBuffer[Vec3[Float32]], x: Int32, y: Int32, z: Int32, n: Int32)
            (using Source): Vec3[Float32] =
  when(inBounds(x, y, z, n)):
    buffer.read(idx3D(x, y, z, n))
  .otherwise:
    vec3(0f, 0f, 0f)
```

These compile to GPU code (no overhead).

## Testing Strategy

### Unit Tests

1. **Zero Velocity Test**: All zeros should remain zeros
2. **Constant Velocity Test**: Uniform velocity should remain uniform
3. **Divergence-Free Test**: After projection, `∇·u < ε`
4. **Conservation Test**: Total mass shouldn't change

### Visual Tests

1. **Smoke Plume**: Should rise smoothly
2. **Vortex Ring**: Should propagate forward
3. **Lid-Driven Cavity**: Compare with benchmark solution

### Performance Tests

1. **Scaling**: Measure FPS for N = 32, 64, 128
2. **Iteration Count**: Quality vs. speed tradeoff
3. **Memory Usage**: Should match theoretical estimate

## Extensions & Future Work

### Near-term

1. **Vorticity Confinement**: Add swirl back
2. **Obstacles**: Solid boundaries in the flow
3. **Multiple Fluids**: Different densities

### Long-term

1. **Multigrid Solver**: 10-100× faster pressure solve
2. **FLIP/PIC Hybrid**: Less dissipation
3. **Adaptive Grids**: Octree or AMR
4. **Surface Tracking**: Level sets for liquid simulation

## Conclusion

This design provides a solid foundation for GPU fluid simulation using Cyfra:

**Strengths**:
- Unconditionally stable
- Fully parallel
- Type-safe
- Composable (GExecution)

**Trade-offs**:
- Uniform grid (memory for sparse regions)
- Iterative solvers (convergence time)
- Some numerical dissipation

**Next Step**: Implementation! See `IMPLEMENTATION_PLAN.md` for detailed steps.



