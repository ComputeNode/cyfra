package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import GridUtils.*

object AdvectionProgram:

  def create: GProgram[Int, FluidStateDouble] =
    GProgram[Int, FluidStateDouble](
      layout = totalCells => {
        FluidStateDouble(
          velocityCurrent = GBuffer[Vec4[Float32]](totalCells),
          pressureCurrent = GBuffer[Float32](totalCells),
          densityCurrent = GBuffer[Float32](totalCells),
          temperatureCurrent = GBuffer[Float32](totalCells),
          divergenceCurrent = GBuffer[Float32](totalCells),
          velocityPrevious = GBuffer[Vec4[Float32]](totalCells),
          pressurePrevious = GBuffer[Float32](totalCells),
          densityPrevious = GBuffer[Float32](totalCells),
          temperaturePrevious = GBuffer[Float32](totalCells),
          divergencePrevious = GBuffer[Float32](totalCells),
          obstacles = GBuffer[Float32](totalCells),
          dyeCurrent = GBuffer[Float32](totalCells),
          dyePrevious = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams]()
        )
      },
      dispatch = (_, totalCells) => {
        val workgroupSize = 256
        val numWorkgroups = (totalCells + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1)
    ): state =>
      val idx = GIO.invocationId
      val params = state.params.read
      val n = params.gridSize
      val totalCells = n * n * n

      GIO.when(idx < totalCells):
        val (x, y, z) = idxTo3D(idx, n)
        val pos = vec3(x.asFloat, y.asFloat, z.asFloat)
        
        // Read velocity from previous buffer (pure operation)
        val vel = GIO.read(state.velocityPrevious, idx)
        
        // Backtrace to previous position (use xyz components only)
        val vel3 = vec3(vel.x, vel.y, vel.z)
        val prevPos = pos - vel3 * params.dt

        // Interpolate values at previous position
        val interpolatedVel = trilinearInterpolateVec4(
          state.velocityPrevious,
          prevPos,
          n
        )

        val interpolatedDensity = trilinearInterpolateFloat32(
          state.densityPrevious,
          prevPos,
          n
        )

        val interpolatedTemperature = trilinearInterpolateFloat32(
          state.temperaturePrevious,
          prevPos,
          n
        )

        val interpolatedDye = trilinearInterpolateFloat32(
          state.dyePrevious,
          prevPos,
          n
        )

        // Write advected values to current buffers
        for
          _ <- GIO.write(state.velocityCurrent, idx, interpolatedVel)
          _ <- GIO.write(state.densityCurrent, idx, interpolatedDensity)
          _ <- GIO.write(state.temperatureCurrent, idx, interpolatedTemperature)
          _ <- GIO.write(state.dyeCurrent, idx, interpolatedDye)
        yield GStruct.Empty()
