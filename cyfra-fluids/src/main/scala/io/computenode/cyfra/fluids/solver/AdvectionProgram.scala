package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState, FluidStateDouble, GridUtils}
import GridUtils.*

object AdvectionProgram:

  def create: GProgram[Int, FluidStateDouble] =
    GProgram[Int, FluidStateDouble](
      layout = totalCells => {
        import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
        FluidStateDouble(
          velocityCurrent = GBuffer[Vec3[Float32]](totalCells),
          pressureCurrent = GBuffer[Float32](totalCells),
          densityCurrent = GBuffer[Float32](totalCells),
          temperatureCurrent = GBuffer[Float32](totalCells),
          divergenceCurrent = GBuffer[Float32](totalCells),
          velocityPrevious = GBuffer[Vec3[Float32]](totalCells),
          pressurePrevious = GBuffer[Float32](totalCells),
          densityPrevious = GBuffer[Float32](totalCells),
          temperaturePrevious = GBuffer[Float32](totalCells),
          divergencePrevious = GBuffer[Float32](totalCells),
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
        // Convert 1D index to 3D coordinates
        val z = idx / (n * n)
        val y = (idx / n).mod(n)
        val x = idx.mod(n)

        val pos = vec3(x.asFloat, y.asFloat, z.asFloat)
        
        // Read velocity from previous buffer (pure operation)
        val vel = GIO.read(state.velocityPrevious, idx)
        
        // Backtrace to previous position
        val prevPos = pos - vel * params.dt

        // Interpolate values at previous position
        val interpolatedVel = trilinearInterpolateVec3(
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

        // Write advected values to current buffers
        for
          _ <- GIO.write(state.velocityCurrent, idx, interpolatedVel)
          _ <- GIO.write(state.densityCurrent, idx, interpolatedDensity)
          _ <- GIO.write(state.temperatureCurrent, idx, interpolatedTemperature)
        yield Empty()
