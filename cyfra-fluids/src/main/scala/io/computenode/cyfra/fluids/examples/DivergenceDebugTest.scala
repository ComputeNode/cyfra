package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}
import io.computenode.cyfra.fluids.core.GridUtils.*
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil
import io.computenode.cyfra.spirvtools.{SpirvCross, SpirvDisassembler, SpirvToolsRunner}
import io.computenode.cyfra.spirvtools.SpirvTool.ToFile
import java.nio.file.Paths

/** Divergence test with printf debugging */
object DivergenceDebugTest:
  
  val divergenceDebugProgram = GProgram[Int, FluidState](
    layout = totalCells => {
      import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
      FluidState(
        velocity = GBuffer[Vec4[Float32]](totalCells),
        pressure = GBuffer[Float32](totalCells),
        density = GBuffer[Float32](totalCells),
        temperature = GBuffer[Float32](totalCells),
        divergence = GBuffer[Float32](totalCells),
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

    // Bounds check to prevent out-of-bounds access
    GIO.when(idx < totalCells):
      // Convert to 3D
      val z = idx / (n * n)
      val y = (idx / n).mod(n)
      val x = idx.mod(n)

      // Read neighbors with bounds checking (returns zero vector if out of bounds)
      val velXP = readVec3Safe(state.velocity, x + 1, y, z, n)
      val velXM = readVec3Safe(state.velocity, x - 1, y, z, n)
      val velYP = readVec3Safe(state.velocity, x, y + 1, z, n)
      val velYM = readVec3Safe(state.velocity, x, y - 1, z, n)
      val velZP = readVec3Safe(state.velocity, x, y, z + 1, n)
      val velZM = readVec3Safe(state.velocity, x, y, z - 1, n)

      // Divergence
      val dx = (velXP.x - velXM.x) * 0.5f
      val dy = (velYP.y - velYM.y) * 0.5f
      val dz = (velZP.z - velZM.z) * 0.5f
      val div = dx + dy + dz

      // All effects in one chain
      for
        // _ <- GIO.when(x === 4 && y === 4 && z === 4)(
        //   for
        //     _ <- GIO.printf("Center [4,4,4] idx=%d\n", idx)
        //     _ <- GIO.printf("  velCenter = (%f, %f, %f)\n", velCenter.x, velCenter.y, velCenter.z)
        //     _ <- GIO.printf("  idxXP=%d, velXP = (%f, %f, %f)\n", idxXP, velXP.x, velXP.y, velXP.z)
        //     _ <- GIO.printf("  idxXM=%d, velXM = (%f, %f, %f)\n", idxXM, velXM.x, velXM.y, velXM.z)
        //     _ <- GIO.printf("  idxYP=%d, velYP = (%f, %f, %f)\n", idxYP, velYP.x, velYP.y, velYP.z)
        //     _ <- GIO.printf("  idxYM=%d, velYM = (%f, %f, %f)\n", idxYM, velYM.x, velYM.y, velYM.z)
        //     _ <- GIO.printf("  idxZP=%d, velZP = (%f, %f, %f)\n", idxZP, velZP.x, velZP.y, velZP.z)
        //     _ <- GIO.printf("  idxZM=%d, velZM = (%f, %f, %f)\n", idxZM, velZM.x, velZM.y, velZM.z)
        //     _ <- GIO.printf("  dx=%f, dy=%f, dz=%f, div=%f\n", dx, dy, dz, div)
        //   yield Empty()
        // )
        _ <- GIO.write(state.divergence, idx, div)
      yield Empty()
  
  @main
  def testDivergenceDebug(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime(spirvToolsRunner =
      SpirvToolsRunner(
        crossCompilation = SpirvCross.Enable(toolOutput = ToFile(Paths.get("output/failing.glsl")), throwOnFail = true),
        disassembler = SpirvDisassembler.Enable(toolOutput = ToFile(Paths.get("output/failing.spvdis"))),
      ),
    )
    
    try
      println("=" * 70)
      println("Divergence Debug Test with Printf")
      println("=" * 70)
      println()
      
      val gridSize = 8
      val totalCells = gridSize * gridSize * gridSize
      
      println(s"Grid: ${gridSize}³ = $totalCells cells")
      println(s"Dispatch: ${(totalCells + 255) / 256} workgroups × 256 threads = ${((totalCells + 255) / 256) * 256} threads")
      println("Creating expanding velocity field...")
      println()
      
      val region = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          divergenceDebugProgram.execute(totalCells, layout)
      
      // Create expanding velocity field
      // Using Vec4 with w=0 for proper 16-byte alignment
      val velData = Array.ofDim[Float](totalCells * 4)
      for i <- 0 until totalCells do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val cx = gridSize / 2.0f
        val cy = gridSize / 2.0f
        val cz = gridSize / 2.0f
        velData(i * 4 + 0) = (x - cx) * 0.2f
        velData(i * 4 + 1) = (y - cy) * 0.2f
        velData(i * 4 + 2) = (z - cz) * 0.2f
        velData(i * 4 + 3) = 0.0f  // w component
      
      // Print what we expect for center cell
      val centerIdx = 4 + 4 * gridSize + 4 * gridSize * gridSize
      println(s"Expected for center cell [4,4,4] (idx=$centerIdx):")
      println(s"  velCenter = (${velData(centerIdx * 4 + 0)}, ${velData(centerIdx * 4 + 1)}, ${velData(centerIdx * 4 + 2)})")
      
      val idxXP = (4+1) + 4 * gridSize + 4 * gridSize * gridSize
      val idxXM = (4-1) + 4 * gridSize + 4 * gridSize * gridSize
      val idxYP = 4 + (4+1) * gridSize + 4 * gridSize * gridSize
      val idxYM = 4 + (4-1) * gridSize + 4 * gridSize * gridSize
      val idxZP = 4 + 4 * gridSize + (4+1) * gridSize * gridSize
      val idxZM = 4 + 4 * gridSize + (4-1) * gridSize * gridSize
      
      println(s"  velXP[5,4,4] = (${velData(idxXP * 4 + 0)}, ${velData(idxXP * 4 + 1)}, ${velData(idxXP * 4 + 2)})")
      println(s"  velXM[3,4,4] = (${velData(idxXM * 4 + 0)}, ${velData(idxXM * 4 + 1)}, ${velData(idxXM * 4 + 2)})")
      println(s"  velYP[4,5,4] = (${velData(idxYP * 4 + 0)}, ${velData(idxYP * 4 + 1)}, ${velData(idxYP * 4 + 2)})")
      println(s"  velYM[4,3,4] = (${velData(idxYM * 4 + 0)}, ${velData(idxYM * 4 + 1)}, ${velData(idxYM * 4 + 2)})")
      println(s"  velZP[4,4,5] = (${velData(idxZP * 4 + 0)}, ${velData(idxZP * 4 + 1)}, ${velData(idxZP * 4 + 2)})")
      println(s"  velZM[4,4,3] = (${velData(idxZM * 4 + 0)}, ${velData(idxZM * 4 + 1)}, ${velData(idxZM * 4 + 2)})")
      
      val expectedDx = (velData(idxXP * 4 + 0) - velData(idxXM * 4 + 0)) * 0.5f
      val expectedDy = (velData(idxYP * 4 + 1) - velData(idxYM * 4 + 1)) * 0.5f
      val expectedDz = (velData(idxZP * 4 + 2) - velData(idxZM * 4 + 2)) * 0.5f
      val expectedDiv = expectedDx + expectedDy + expectedDz
      
      println(s"  Expected: dx=$expectedDx, dy=$expectedDy, dz=$expectedDz, div=$expectedDiv")
      println()
      println("GPU Output (from printf):")
      println("-" * 70)
      
      val velBuffer = BufferUtils.createByteBuffer(velData.length * 4)
      velBuffer.asFloatBuffer().put(velData).flip()
      
      println(s"Buffer sizes:")
      println(s"  velocity: ${velBuffer.capacity()} bytes (${velBuffer.capacity() / 16} Vec4 elements)")
      println(s"  params: 28 bytes (FluidParams struct)")
      println()
      
      val paramsBuffer = BufferUtils.createByteBuffer(28)
      paramsBuffer.putFloat(0.1f)       // dt
      paramsBuffer.putFloat(0.0f)       // viscosity
      paramsBuffer.putFloat(0.0f)       // diffusion
      paramsBuffer.putFloat(1.0f)       // buoyancy
      paramsBuffer.putFloat(0.0f)       // ambient
      paramsBuffer.putInt(gridSize)     // gridSize
      paramsBuffer.putInt(1)            // iterationCount
      paramsBuffer.flip()
      
      val divResultBuffer = BufferUtils.createFloatBuffer(totalCells)
      val divResultBB = MemoryUtil.memByteBuffer(divResultBuffer)
      
      region.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec4[Float32]](velBuffer),
          pressure = GBuffer[Float32](totalCells),       // Output: just size
          density = GBuffer[Float32](totalCells),        // Output: just size
          temperature = GBuffer[Float32](totalCells),    // Output: just size
          divergence = GBuffer[Float32](totalCells),     // Output: just size
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.divergence.read(divResultBB)
      )
      
      println("-" * 70)
      println()
      println(s"Actual divergence at center [4,4,4]: ${divResultBuffer.get(centerIdx)}")
      println()
      println("Sample divergences:")
      for i <- 0 until Math.min(10, totalCells) do
        println(s"  Cell $i: div=${divResultBuffer.get(i)}")
      println()
      println(s"Max divergence: ${(0 until totalCells).map(divResultBuffer.get).max}")
      println(s"Min divergence: ${(0 until totalCells).map(divResultBuffer.get).min}")
      println("=" * 70)
      
    finally
      runtime.close()

