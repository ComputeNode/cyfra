package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}
import io.computenode.cyfra.fluids.core.GridUtils.*
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Test divergence with direct buffer reads (no when/otherwise) */
object DivergenceDirectTest:
  
  val divergenceDirectProgram = GProgram[Int, FluidState](
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
    dispatch = (_, totalCells) => StaticDispatch((totalCells / 256 + 1, 1, 1)),
    workgroupSize = (256, 1, 1)
  ): state =>
    val idx = GIO.invocationId
    val params = state.params.read
    val n = params.gridSize

    // Convert 1D to 3D
    val z = idx / (n * n)
    val y = (idx / n).mod(n)
    val x = idx.mod(n)

    // Direct neighbor indices (no bounds checking - will wrap or read garbage at edges)
    val idxXP = idx3D(x + 1, y, z, n)
    val idxXM = idx3D(x - 1, y, z, n)
    val idxYP = idx3D(x, y + 1, z, n)
    val idxYM = idx3D(x, y - 1, z, n)
    val idxZP = idx3D(x, y, z + 1, n)
    val idxZM = idx3D(x, y, z - 1, n)

    // Read neighbors directly
    val velXP = GIO.read(state.velocity, idxXP)
    val velXM = GIO.read(state.velocity, idxXM)
    val velYP = GIO.read(state.velocity, idxYP)
    val velYM = GIO.read(state.velocity, idxYM)
    val velZP = GIO.read(state.velocity, idxZP)
    val velZM = GIO.read(state.velocity, idxZM)

    // Divergence
    val dx = (velXP.x - velXM.x) * 0.5f
    val dy = (velYP.y - velYM.y) * 0.5f
    val dz = (velZP.z - velZM.z) * 0.5f

    val div = dx + dy + dz

    GIO.write(state.divergence, idx, div)
  
  @main
  def testDivergenceDirect(): Unit =
    import io.computenode.cyfra.spirvtools.{SpirvToolsRunner, SpirvDisassembler, SpirvCross}
    import io.computenode.cyfra.spirvtools.SpirvTool.ToFile
    import java.nio.file.Paths
    
    given runtime: VkCyfraRuntime = VkCyfraRuntime(
      spirvToolsRunner = SpirvToolsRunner(
        disassembler = SpirvDisassembler.Enable(
          toolOutput = ToFile(Paths.get("output/divergence_debug.spvasm"))
        ),
        crossCompilation = SpirvCross.Enable(
          toolOutput = ToFile(Paths.get("output/divergence_debug.glsl"))
        )
      )
    )
    
    try
      println("=" * 70)
      println("Divergence Direct Test (No Bounds Checking)")
      println("=" * 70)
      println()
      
      val gridSize = 8
      val totalCells = gridSize * gridSize * gridSize
      
      println(s"Grid: ${gridSize}³ = $totalCells cells")
      println()
      
      val region = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          divergenceDirectProgram.execute(totalCells, layout)
      
      // Create expanding velocity field (Vec4 with w=0)
      val velData = Array.ofDim[Float](totalCells * 4)
      for i <- 0 until totalCells do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val cx = gridSize / 2.0f
        val cy = gridSize / 2.0f
        val cz = gridSize / 2.0f
        // Velocity pointing away from center (positive divergence)
        velData(i * 4 + 0) = (x - cx) * 0.2f
        velData(i * 4 + 1) = (y - cy) * 0.2f
        velData(i * 4 + 2) = (z - cz) * 0.2f
        velData(i * 4 + 3) = 0.0f  // w component
      
      println("Velocity field: expanding from center")
      println("Expected: positive divergence in most cells")
      println()
      
      val velBuffer = BufferUtils.createByteBuffer(velData.length * 4)
      velBuffer.asFloatBuffer().put(velData).flip()
      
      val paramsBuffer = BufferUtils.createByteBuffer(32)
      paramsBuffer.putFloat(0.1f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(1.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putInt(gridSize)
      paramsBuffer.putInt(1)
      paramsBuffer.flip()
      
      val divResultBuffer = BufferUtils.createFloatBuffer(totalCells)
      val divResultBB = MemoryUtil.memByteBuffer(divResultBuffer)
      
      region.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec4[Float32]](velBuffer),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.divergence.read(divResultBB)
      )
      
      // Analyze
      val divs = (0 until totalCells).map(i => divResultBuffer.get(i))
      val nonZero = divs.count(d => Math.abs(d) > 0.001f)
      val positive = divs.count(_ > 0.001f)
      val avgDiv = divs.sum / totalCells
      val maxDiv = divs.max
      val minDiv = divs.min
      
      println("RESULTS:")
      println(s"  Non-zero divergence cells: $nonZero / $totalCells")
      println(s"  Positive divergence cells: $positive")
      println(s"  Average divergence: $avgDiv")
      println(s"  Divergence range: [$minDiv, $maxDiv]")
      println()
      
      if nonZero > totalCells / 4 && positive > nonZero / 2 then
        println("✅ SUCCESS! Divergence computation works!")
        println()
        println("Center cells:")
        for z <- (gridSize/2 - 1) to (gridSize/2 + 1)
            y <- (gridSize/2 - 1) to (gridSize/2 + 1)
            x <- (gridSize/2 - 1) to (gridSize/2 + 1)
        do
          val idx = x + y * gridSize + z * gridSize * gridSize
          if idx < totalCells then
            val div = divResultBuffer.get(idx)
            if Math.abs(div) > 0.001f then
              println(s"  [$x,$y,$z]: div=$div")
      else
        println("❌ FAILED: Divergence incorrect")
        println()
        println("Debug: Center cells:")
        for z <- (gridSize/2 - 1) to (gridSize/2 + 1)
            y <- (gridSize/2 - 1) to (gridSize/2 + 1) if y == gridSize/2
            x <- (gridSize/2 - 1) to (gridSize/2 + 1) if x == gridSize/2
        do
          val idx = x + y * gridSize + z * gridSize * gridSize
          if idx < totalCells then
            val div = divResultBuffer.get(idx)
            val vx = velData(idx * 4 + 0)
            val vy = velData(idx * 4 + 1)
            val vz = velData(idx * 4 + 2)
            println(s"  [$x,$y,$z]: vel=($vx,$vy,$vz), div=$div")
      
      println("=" * 70)
      
    finally
      runtime.close()

