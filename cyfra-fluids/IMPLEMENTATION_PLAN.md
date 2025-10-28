# 3D Navier-Stokes Fluid Simulation with Cyfra - Implementation Plan

## Overview

This document outlines the implementation of a GPU-accelerated 3D incompressible fluid simulator using Cyfra, based on Jos Stam's "Stable Fluids" method, with real-time visualization using ray marching.

## Theoretical Background

### Navier-Stokes Equations

The incompressible Navier-Stokes equations govern fluid motion:

```
∂u/∂t = -(u·∇)u - ∇p/ρ + ν∇²u + f
∇·u = 0  (incompressibility constraint)
```

Where:
- `u` = velocity field (Vec3)
- `p` = pressure
- `ρ` = density
- `ν` = kinematic viscosity
- `f` = external forces

### Numerical Method: Operator Splitting

We'll use Jos Stam's stable fluids approach, splitting the solution into sequential steps:

1. **Add Forces**: Apply external forces (gravity, user input)
2. **Advection**: Transport quantities by the velocity field (semi-Lagrangian)
3. **Diffusion**: Apply viscosity (implicit solve)
4. **Projection**: Enforce incompressibility (Helmholtz-Hodge decomposition)

Each step is unconditionally stable, allowing large time steps.

## Architecture

### Module Structure

```
cyfra-fluids/
├── src/main/scala/io/computenode/cyfra/fluids/
│   ├── core/
│   │   ├── FluidGrid3D.scala           # 3D grid data structure
│   │   ├── FluidState.scala            # Velocity, pressure, density fields
│   │   └── FluidParameters.scala       # Simulation parameters
│   ├── solver/
│   │   ├── NavierStokesSolver.scala    # Main solver orchestration
│   │   ├── AdvectionProgram.scala      # Semi-Lagrangian advection
│   │   ├── DiffusionProgram.scala      # Jacobi iteration for diffusion
│   │   ├── ProjectionProgram.scala     # Pressure projection (Poisson solver)
│   │   └── BoundaryProgram.scala       # Boundary conditions
│   ├── visualization/
│   │   ├── RayMarchRenderer.scala      # Volume ray marching
│   │   ├── FluidColorMap.scala         # Density to color mapping
│   │   └── Camera3D.scala              # Camera controls
│   └── examples/
│       ├── SmokeDemo.scala             # Rising smoke simulation
│       ├── VortexDemo.scala            # Vortex ring simulation
│       └── InteractiveDemo.scala       # User-controlled forces
```

### Dependencies

Add to `build.sbt`:

```scala
lazy val fluids = project
  .in(file("cyfra-fluids"))
  .dependsOn(runtime, foton)
  .settings(commonSettings)
  .settings(
    name := "cyfra-fluids",
    libraryDependencies ++= Seq(
      // Already available from cyfra-core
    )
  )
```

## Data Structures

### 1. Grid Representation

```scala
case class FluidGrid(
  size: Int32,          // Grid resolution (size³ voxels)
  cellSize: Float32     // Physical size of each cell
) extends GStruct[FluidGrid]

// 3D indexing helper (flattened to 1D)
def idx3D(x: Int32, y: Int32, z: Int32, size: Int32): Int32 =
  x + y * size + z * size * size

// Boundary check
def inBounds(x: Int32, y: Int32, z: Int32, size: Int32): GBoolean =
  (x >= 0) && (x < size) && (y >= 0) && (y < size) && (z >= 0) && (z < size)
```

### 2. Fluid State

```scala
case class FluidState(
  velocity: GBuffer[Vec3[Float32]],  // Velocity field (u, v, w)
  pressure: GBuffer[Float32],         // Pressure field
  density: GBuffer[Float32],          // Smoke/dye density
  temperature: GBuffer[Float32]       // Temperature (for buoyancy)
) extends Layout

case class FluidStateDouble(
  current: FluidState,
  previous: FluidState  // Double buffering for ping-pong
) extends Layout
```

### 3. Simulation Parameters

```scala
case class FluidParams(
  dt: Float32,              // Time step
  viscosity: Float32,       // Kinematic viscosity (ν)
  diffusion: Float32,       // Density diffusion rate
  buoyancy: Float32,        // Buoyancy force (smoke rises)
  ambient: Float32,         // Ambient temperature
  gridSize: Int32,          // Grid resolution
  iterationCount: Int32     // Jacobi iterations for solvers
) extends GStruct[FluidParams]
```

## Implementation Steps

### Step 1: Add Forces

Apply external forces and buoyancy (rising smoke due to temperature):

```scala
val addForcesProgram = GProgram[FluidParams, FluidState](
  layout = params => FluidState(...),
  dispatch = (layout, params) => {
    val n = params.gridSize
    StaticDispatch(((n * n * n + 255) / 256, 1, 1))
  },
  workgroupSize = (256, 1, 1)
): state =>
  val idx = GIO.invocationId
  val params = state.params.read
  
  when(idx < params.gridSize * params.gridSize * params.gridSize):
    val oldVel = state.velocity.read(idx)
    val temp = state.temperature.read(idx)
    
    // Buoyancy: hot fluid rises (thermal Boussinesq approximation)
    val buoyancyForce = vec3(
      0.0f,
      params.buoyancy * (temp - params.ambient),
      0.0f
    )
    
    // F_total = F_external + F_buoyancy
    val newVel = oldVel + buoyancyForce * params.dt
    
    for _ <- state.velocity.write(idx, newVel)
    yield ()
```

### Step 2: Advection (Semi-Lagrangian)

Transport quantities backward in time along velocity field:

```scala
val advectionProgram = GProgram[FluidParams, FluidStateDouble](
  layout = params => FluidStateDouble(...),
  dispatch = ...,
  workgroupSize = (256, 1, 1)
): state =>
  val idx = GIO.invocationId
  val params = state.params.read
  val n = params.gridSize
  
  when(idx < n * n * n):
    // Convert 1D index to 3D coordinates
    val z = idx / (n * n)
    val y = (idx / n) % n
    val x = idx % n
    
    val pos = vec3(x.asFloat, y.asFloat, z.asFloat)
    val vel = state.current.velocity.read(idx)
    
    // Backtrace: where did this particle come from?
    val prevPos = pos - vel * params.dt
    
    // Trilinear interpolation at prevPos
    val interpolatedVel = trilinearInterpolate(
      state.previous.velocity, 
      prevPos, 
      n
    )
    
    val interpolatedDensity = trilinearInterpolate(
      state.previous.density,
      prevPos,
      n
    )
    
    for
      _ <- state.current.velocity.write(idx, interpolatedVel)
      _ <- state.current.density.write(idx, interpolatedDensity)
    yield ()
```

Trilinear interpolation helper (DSL function):

```scala
def trilinearInterpolate(
  buffer: GBuffer[Vec3[Float32]], 
  pos: Vec3[Float32], 
  size: Int32
)(using Source): Vec3[Float32] =
  val x = clamp(pos.x, 0.0f, (size - 1).asFloat)
  val y = clamp(pos.y, 0.0f, (size - 1).asFloat)
  val z = clamp(pos.z, 0.0f, (size - 1).asFloat)
  
  val x0 = floor(x).asInt
  val y0 = floor(y).asInt
  val z0 = floor(z).asInt
  val x1 = min(x0 + 1, size - 1)
  val y1 = min(y0 + 1, size - 1)
  val z1 = min(z0 + 1, size - 1)
  
  val fx = x - x0.asFloat
  val fy = y - y0.asFloat
  val fz = z - z0.asFloat
  
  // Sample 8 corners of the cube
  val v000 = buffer.read(idx3D(x0, y0, z0, size))
  val v100 = buffer.read(idx3D(x1, y0, z0, size))
  val v010 = buffer.read(idx3D(x0, y1, z0, size))
  val v110 = buffer.read(idx3D(x1, y1, z0, size))
  val v001 = buffer.read(idx3D(x0, y0, z1, size))
  val v101 = buffer.read(idx3D(x1, y0, z1, size))
  val v011 = buffer.read(idx3D(x0, y1, z1, size))
  val v111 = buffer.read(idx3D(x1, y1, z1, size))
  
  // Trilinear interpolation
  val v00 = mix(v000, v100, fx)
  val v10 = mix(v010, v110, fx)
  val v01 = mix(v001, v101, fx)
  val v11 = mix(v011, v111, fx)
  
  val v0 = mix(v00, v10, fy)
  val v1 = mix(v01, v11, fy)
  
  mix(v0, v1, fz)
```

### Step 3: Diffusion (Jacobi Iteration)

Solve implicit diffusion equation `(I - ν·Δt·∇²)u^{n+1} = u^n`:

```scala
val diffusionProgram = GProgram[FluidParams, FluidStateDouble](
  layout = params => FluidStateDouble(...),
  dispatch = ...,
  workgroupSize = (256, 1, 1)
): state =>
  val idx = GIO.invocationId
  val params = state.params.read
  val n = params.gridSize
  
  when(idx < n * n * n):
    val z = idx / (n * n)
    val y = (idx / n) % n
    val x = idx % n
    
    // Jacobi iteration: average of neighbors
    val alpha = 1.0f / (params.viscosity * params.dt)
    val beta = 1.0f / (6.0f + alpha)
    
    val center = state.previous.velocity.read(idx)
    
    // Read 6 neighbors (with boundary handling)
    val neighbors = getNeighbors6(state.previous.velocity, x, y, z, n)
    val neighborSum = neighbors._1 + neighbors._2 + neighbors._3 + 
                      neighbors._4 + neighbors._5 + neighbors._6
    
    val newVel = (center * alpha + neighborSum) * beta
    
    for _ <- state.current.velocity.write(idx, newVel)
    yield ()
```

### Step 4: Pressure Projection (Helmholtz-Hodge Decomposition)

Enforce incompressibility `∇·u = 0` by subtracting pressure gradient:

**Sub-step 4a**: Compute divergence of velocity:

```scala
val divergenceProgram = GProgram[FluidParams, FluidState](
  layout = ...,
  dispatch = ...,
  workgroupSize = (256, 1, 1)
): state =>
  val idx = GIO.invocationId
  val params = state.params.read
  val n = params.gridSize
  
  when(idx < n * n * n):
    val z = idx / (n * n)
    val y = (idx / n) % n
    val x = idx % n
    
    // Central differences for divergence
    val velXp = readVelocity(state.velocity, x + 1, y, z, n).x
    val velXm = readVelocity(state.velocity, x - 1, y, z, n).x
    val velYp = readVelocity(state.velocity, x, y + 1, z, n).y
    val velYm = readVelocity(state.velocity, x, y - 1, z, n).y
    val velZp = readVelocity(state.velocity, x, y, z + 1, n).z
    val velZm = readVelocity(state.velocity, x, y, z - 1, n).z
    
    val div = ((velXp - velXm) + (velYp - velYm) + (velZp - velZm)) * 0.5f
    
    for _ <- state.divergence.write(idx, div)
    yield ()
```

**Sub-step 4b**: Solve Poisson equation `∇²p = ∇·u` using Jacobi:

```scala
val pressureSolveProgram = GProgram[FluidParams, FluidStateDouble](
  layout = ...,
  dispatch = ...,
  workgroupSize = (256, 1, 1)
): state =>
  val idx = GIO.invocationId
  val params = state.params.read
  val n = params.gridSize
  
  when(idx < n * n * n):
    val z = idx / (n * n)
    val y = (idx / n) % n
    val x = idx % n
    
    val divergence = state.divergence.read(idx)
    
    // Jacobi iteration for Poisson equation
    val neighbors = getNeighborsPressure(state.previous.pressure, x, y, z, n)
    val neighborSum = neighbors._1 + neighbors._2 + neighbors._3 + 
                      neighbors._4 + neighbors._5 + neighbors._6
    
    val newPressure = (neighborSum - divergence) / 6.0f
    
    for _ <- state.current.pressure.write(idx, newPressure)
    yield ()
```

**Sub-step 4c**: Subtract pressure gradient from velocity:

```scala
val projectionProgram = GProgram[FluidParams, FluidState](
  layout = ...,
  dispatch = ...,
  workgroupSize = (256, 1, 1)
): state =>
  val idx = GIO.invocationId
  val params = state.params.read
  val n = params.gridSize
  
  when(idx < n * n * n):
    val z = idx / (n * n)
    val y = (idx / n) % n
    val x = idx % n
    
    val vel = state.velocity.read(idx)
    
    // Compute pressure gradient
    val pXp = readPressure(state.pressure, x + 1, y, z, n)
    val pXm = readPressure(state.pressure, x - 1, y, z, n)
    val pYp = readPressure(state.pressure, x, y + 1, z, n)
    val pYm = readPressure(state.pressure, x, y - 1, z, n)
    val pZp = readPressure(state.pressure, x, y, z + 1, n)
    val pZm = readPressure(state.pressure, x, y, z - 1, n)
    
    val gradP = vec3(
      (pXp - pXm) * 0.5f,
      (pYp - pYm) * 0.5f,
      (pZp - pZm) * 0.5f
    )
    
    val divergenceFreeVel = vel - gradP
    
    for _ <- state.velocity.write(idx, divergenceFreeVel)
    yield ()
```

### Step 5: Boundary Conditions

Apply no-slip or free-slip boundaries:

```scala
val boundaryProgram = GProgram[FluidParams, FluidState](
  layout = ...,
  dispatch = ...,
  workgroupSize = (256, 1, 1)
): state =>
  val idx = GIO.invocationId
  val params = state.params.read
  val n = params.gridSize
  
  when(idx < n * n * n):
    val z = idx / (n * n)
    val y = (idx / n) % n
    val x = idx % n
    
    // Check if on boundary
    val onBoundary = (x === 0) || (x === n - 1) ||
                     (y === 0) || (y === n - 1) ||
                     (z === 0) || (z === n - 1)
    
    when(onBoundary):
      // No-slip: zero velocity at walls
      // Or free-slip: reflect normal component
      val vel = state.velocity.read(idx)
      val boundaryVel = vec3(0.0f, 0.0f, 0.0f)  // No-slip
      
      for _ <- state.velocity.write(idx, boundaryVel)
      yield ()
```

## Solver Orchestration

Use `GExecution` to chain all passes:

```scala
class NavierStokesSolver(params: FluidParams)(using CyfraRuntime):
  
  def step(state: FluidStateDouble): FluidStateDouble =
    val execution = GExecution[FluidParams, FluidStateDouble]()
      .addProgram(addForcesProgram)(
        mapParams = identity,
        mapLayout = _.current
      )
      .addProgram(advectionProgram)(
        mapParams = identity,
        mapLayout = identity
      )
      // Diffusion iterations (repeat 20 times)
      .addProgram(diffusionIteration(20))(
        mapParams = identity,
        mapLayout = identity
      )
      // Pressure projection
      .addProgram(divergenceProgram)(
        mapParams = identity,
        mapLayout = _.current
      )
      .addProgram(pressureSolveIteration(40))(
        mapParams = identity,
        mapLayout = identity
      )
      .addProgram(projectionProgram)(
        mapParams = identity,
        mapLayout = _.current
      )
      .addProgram(boundaryProgram)(
        mapParams = identity,
        mapLayout = _.current
      )
    
    execution.execute(params, state)
  
  private def diffusionIteration(count: Int): GProgram[...] = ...
  private def pressureSolveIteration(count: Int): GProgram[...] = ...
```

## Visualization

### Ray Marching Volume Renderer

```scala
class FluidRenderer(resolution: (Int, Int))(using CyfraRuntime):
  
  def renderFrame(
    fluidState: FluidState,
    camera: Camera3D
  ): Array[Vec4[Float32]] =
    
    val renderProgram = GProgram[(Camera3D, FluidState), RenderLayout](
      layout = (cam, state) => RenderLayout(
        output = GBuffer[Vec4[Float32]](resolution._1 * resolution._2),
        fluidDensity = state.density,
        cameraParams = GUniform(cam)
      ),
      dispatch = (layout, _) => {
        val w = resolution._1
        val h = resolution._2
        StaticDispatch(((w * h + 255) / 256, 1, 1))
      },
      workgroupSize = (256, 1, 1)
    ): layout =>
      val idx = GIO.invocationId
      val w = resolution._1
      val h = resolution._2
      
      when(idx < w * h):
        val y = idx / w
        val x = idx % w
        
        val uv = vec2(
          (x.asFloat / w.toFloat) * 2.0f - 1.0f,
          (y.asFloat / h.toFloat) * 2.0f - 1.0f
        )
        
        val cam = layout.cameraParams.read
        val ray = generateRay(uv, cam)
        
        // Ray march through volume
        val color = raymarch(
          ray.origin,
          ray.direction,
          layout.fluidDensity,
          params.gridSize
        )
        
        for _ <- layout.output.write(idx, color)
        yield ()
    
    renderProgram.run((camera, fluidState))
```

### Ray Marching Algorithm

```scala
def raymarch(
  origin: Vec3[Float32],
  direction: Vec3[Float32],
  density: GBuffer[Float32],
  gridSize: Int32
)(using Source): Vec4[Float32] =
  
  case class RayState(
    pos: Vec3[Float32],
    accumColor: Vec4[Float32],
    transmittance: Float32,
    t: Float32
  )
  
  val maxSteps = 128
  val stepSize = 0.05f
  
  val finalState = GSeq
    .gen(
      first = RayState(origin, vec4(0.0f, 0.0f, 0.0f, 0.0f), 1.0f, 0.0f),
      next = state => {
        val nextPos = state.pos + direction * stepSize
        val d = sampleDensity(density, nextPos, gridSize)
        
        // Color based on density
        val sampleColor = densityToColor(d)
        val alpha = d * 0.1f
        
        val newColor = state.accumColor + sampleColor * alpha * state.transmittance
        val newTransmittance = state.transmittance * (1.0f - alpha)
        
        RayState(nextPos, newColor, newTransmittance, state.t + stepSize)
      }
    )
    .limit(maxSteps)
    .takeWhile(state => (state.transmittance > 0.01f) && (state.t < 5.0f))
    .lastOr(RayState(origin, vec4(0.0f, 0.0f, 0.0f, 0.0f), 0.0f, 0.0f))
  
  finalState.accumColor
```

## Demo Applications

### 1. Rising Smoke

```scala
object SmokeDemo:
  @main def smokeDemo() =
    given CyfraRuntime = VkCyfraRuntime()
    
    val gridSize = 64
    val params = FluidParams(
      dt = 0.1f,
      viscosity = 0.0001f,
      diffusion = 0.001f,
      buoyancy = 0.5f,
      ambient = 0.0f,
      gridSize = gridSize,
      iterationCount = 20
    )
    
    val solver = NavierStokesSolver(params)
    val renderer = FluidRenderer((512, 512))
    
    // Initialize with smoke source at bottom
    var state = initializeSmoke(gridSize)
    
    for frame <- 0 until 300 do
      // Add smoke at bottom center
      state = addSmokeSource(state, gridSize)
      
      // Simulate
      state = solver.step(state)
      
      // Render
      val image = renderer.renderFrame(state.current, camera)
      exportFrame(image, s"smoke_$frame.png")
```

### 2. Interactive Demo with User Forces

```scala
object InteractiveDemo:
  @main def interactiveDemo() =
    // Use window framework (GLFW) for real-time interaction
    // Click and drag to apply forces
    // Spacebar to add smoke
    // Arrow keys to move camera
```

## Performance Optimizations

1. **Multigrid Solver**: Replace Jacobi iterations with multigrid for faster pressure solve
2. **Shared Memory**: Use workgroup local memory for neighbor access
3. **Adaptive Time Stepping**: Adjust dt based on CFL condition
4. **Level of Detail**: Lower resolution for distant volumes
5. **Async Compute**: Overlap simulation and rendering

## Testing Strategy

1. **Unit Tests**: Each program individually (constant velocity, zero divergence)
2. **Convergence Tests**: Verify Jacobi iterations converge
3. **Conservation Tests**: Check mass/momentum conservation
4. **Visual Tests**: Compare with known fluid behaviors

## Timeline Estimate

| Task | Estimated Time |
|------|---------------|
| Data structures & grid indexing | 2 hours |
| Advection program | 2 hours |
| Diffusion program | 2 hours |
| Pressure projection | 4 hours |
| Boundary conditions | 1 hour |
| Solver orchestration | 2 hours |
| Ray marching visualization | 4 hours |
| Demo applications | 3 hours |
| Testing & debugging | 4 hours |
| **Total** | **24 hours** |

## Success Criteria

- [ ] 64³ grid runs at 30+ FPS
- [ ] Smoke plumes rise realistically
- [ ] Vortices persist and rotate
- [ ] No visible numerical artifacts
- [ ] Interactive forces affect fluid
- [ ] Volume rendering shows detail

## References

- Jos Stam, "Stable Fluids" (SIGGRAPH 1999)
- Bridson & Müller-Fischer, "Fluid Simulation" (SIGGRAPH Course Notes)
- GPU Gems Chapter 38: Fast Fluid Dynamics Simulation
- Real-Time Fluid Dynamics for Games (Stam, 2003)

## Next Steps

Once basic implementation works:
1. Add vorticity confinement for more detail
2. Implement FLIP/PIC hybrid for particle-grid methods
3. Add multiple fluid layers (density stratification)
4. Obstacle handling (solid boundaries)
5. Export to VDB format for offline rendering



