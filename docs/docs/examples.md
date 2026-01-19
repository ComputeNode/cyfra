---
sidebar_position: 7
---

# Examples

## Ray Tracing

![Animated Raytracing](https://github.com/user-attachments/assets/3eac9f7f-72df-4a5d-b768-9117d651c78d)

A GPU-accelerated path tracer with support for reflections, refractions, and soft shadows. The Foton library provides an API for defining materials, shapes, and animated camera movements.

**Features:**
- Physically-based materials (diffuse, specular, refractive)
- Sphere, plane, box, and quad primitives
- Soft shadows and global illumination
- Animation support with smooth interpolation

[View the example](https://github.com/ComputeNode/cyfra/blob/main/cyfra-examples/src/main/scala/io/computenode/cyfra/samples/foton/AnimatedRaytrace.scala) | [Ray tracing implementation](https://github.com/ComputeNode/cyfra/blob/main/cyfra-foton/src/main/scala/io/computenode/cyfra/foton/rt/RtRenderer.scala)

---

## Navier-Stokes Fluid Simulation

![Fluid Simulation](/img/full_fluid_8s.gif)

A 3D incompressible Navier-Stokes fluid solver running entirely on GPU. Uses GExecution pipelines to chain multiple solver stages: forces, advection, diffusion, pressure projection, and boundary conditions.

**Features:**
- 3D velocity, pressure, density, and temperature fields
- Buoyancy and vorticity confinement forces
- Jacobi iteration pressure solver
- Volume ray-marching visualization
- Double-buffered state for stable advection

[View the implementation](https://github.com/ComputeNode/cyfra/tree/main/cyfra-fluids/src/main/scala/io/computenode/cyfra/fluids)

---

## Customer Analytics Web App

![Clustering Animation](/img/clustering.gif)

A real-time customer segmentation service using GPU-accelerated Fuzzy C-Means clustering. Demonstrates how to build web applications with Cyfra, including HTTP endpoints and streaming data processing.

**Features:**
- Integration with fs2 streams
- GPU-accelerated FCM clustering algorithm
- REST API for real-time segmentation
- Feature extraction from customer profiles
- Animated visualization of cluster membership

[View the server](https://github.com/ComputeNode/cyfra/blob/main/cyfra-analytics/src/main/scala/io/computenode/cyfra/analytics/server/SegmentationServer.scala) | [GPU pipeline](https://github.com/ComputeNode/cyfra/blob/main/cyfra-analytics/src/main/scala/io/computenode/cyfra/analytics/gpu/GpuAnalyticsPipeline.scala)

---

## Animated Julia Set

<img src="https://raw.githubusercontent.com/ComputeNode/cyfra/main/assets/julia.gif" width="400" />

Animation of the Julia set fractal with smoothly varying parameters. Showcases the Foton animation library's ability to animate mathematical functions.

**Features:**
- GSeq-based fractal iteration
- Smooth parameter interpolation
- Custom color mapping

[View the example](https://github.com/ComputeNode/cyfra/blob/main/cyfra-examples/src/main/scala/io/computenode/cyfra/samples/foton/AnimatedJulia.scala)