package io.computenode.cyfra.fluids.solver.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.*
import io.computenode.cyfra.fluids.solver.utils.{GridUtils, ObstacleUtils}
import GridUtils.idxTo3D

/** Proper free-slip boundary conditions - fluid slides tangentially along surfaces.
  *
  * Removes only the normal component of velocity, preserving tangential flow. This allows fluid to slide freely along walls and obstacles.
  */
object FreeSlipBoundaryProgram:

  def create: GProgram[Int, FluidState] =
    GProgram[Int, FluidState](
      layout = totalCells =>
        FluidState(
          velocity = GBuffer[Vec4[Float32]](totalCells),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          obstacles = GBuffer[Float32](totalCells),
          dye = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](),
        ),
      dispatch = (_, totalCells) => {
        val workgroupSize = 256
        val numWorkgroups = (totalCells + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): state =>
      val idx = GIO.invocationId
      val params = state.params.read
      val n = params.gridSize
      val totalCells = n * n * n

      GIO.when(idx < totalCells):
        val (x, y, z) = idxTo3D(idx, n)

        val onDomainBoundary =
          (x === 0) ||
            (x === n - 1) ||
            (y === 0) ||
            (y === n - 1) ||
            (z === 0) ||
            (z === n - 1)
        val isSolid = ObstacleUtils.isSolid(state.obstacles, idx, totalCells)

        val solidXP = ObstacleUtils.isSolidAt(state.obstacles, x + 1, y, z, n)
        val solidXM = ObstacleUtils.isSolidAt(state.obstacles, x - 1, y, z, n)
        val solidYP = ObstacleUtils.isSolidAt(state.obstacles, x, y + 1, z, n)
        val solidYM = ObstacleUtils.isSolidAt(state.obstacles, x, y - 1, z, n)
        val solidZP = ObstacleUtils.isSolidAt(state.obstacles, x, y, z + 1, n)
        val solidZM = ObstacleUtils.isSolidAt(state.obstacles, x, y, z - 1, n)
        val adjacentToObstacle = (solidXP || solidXM || solidYP || solidYM || solidZP || solidZM) && !isSolid

        for
          _ <- GIO.when(isSolid):
            for
              _ <- GIO.write(state.velocity, idx, vec4(0.0f, 0.0f, 0.0f, 0.0f))
              _ <- GIO.write(state.density, idx, 0.0f)
              _ <- GIO.write(state.temperature, idx, 0.0f)
            yield GStruct.Empty()

          _ <- GIO.when(adjacentToObstacle && !onDomainBoundary):
            val vel = state.velocity.read(idx)
            val vel3 = vec3(vel.x, vel.y, vel.z)

            val nx = when(solidXP)(1.0f).otherwise(0.0f) + when(solidXM)(-1.0f).otherwise(0.0f)
            val ny = when(solidYP)(1.0f).otherwise(0.0f) + when(solidYM)(-1.0f).otherwise(0.0f)
            val nz = when(solidZP)(1.0f).otherwise(0.0f) + when(solidZM)(-1.0f).otherwise(0.0f)
            val obstacleNormal = vec3(nx, ny, nz)

            val normalLength = sqrt((obstacleNormal dot obstacleNormal) + 1e-8f)
            val normalNorm = obstacleNormal * (1.0f / normalLength)

            val normalComponent = vel3 dot normalNorm
            val tangentialVel = vel3 - (normalNorm * normalComponent)

            val newVel = vec4(tangentialVel.x, tangentialVel.y, tangentialVel.z, 0.0f)
            GIO.write(state.velocity, idx, newVel)

          _ <- GIO.when(onDomainBoundary && !isSolid && !adjacentToObstacle):
            val vel = state.velocity.read(idx)
            val vel3 = vec3(vel.x, vel.y, vel.z)

            val nx = when(x === 0)(1.0f).elseWhen(x === n - 1)(-1.0f).otherwise(0.0f)
            val ny = when(y === 0)(1.0f).elseWhen(y === n - 1)(-1.0f).otherwise(0.0f)
            val nz = when(z === 0)(1.0f).elseWhen(z === n - 1)(-1.0f).otherwise(0.0f)
            val normal = vec3(nx, ny, nz)

            val normalLength = sqrt((normal dot normal) + 1e-8f)
            val normalNorm = normal * (1.0f / normalLength)

            val normalComponent = vel3 dot normalNorm
            val tangentialVel = vel3 - (normalNorm * normalComponent)

            val newVel = vec4(tangentialVel.x, tangentialVel.y, tangentialVel.z, 0.0f)
            GIO.write(state.velocity, idx, newVel)
        yield GStruct.Empty()
