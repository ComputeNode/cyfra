package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Test ForcesProgram logic without GIO.when bounds checking */
object ForcesProgramDirectTest:
  
  // Forces program WITHOUT GIO.when condition
  val forcesDirectProgram = GProgram[Int, FluidState](
    layout = totalCells => {
      import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
      FluidState(
        velocity = GBuffer[Vec3[Float32]](totalCells),
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
    
    // NO GIO.when - just do the calculation directly
    val oldVel = GIO.read(state.velocity, idx)
    val temp = GIO.read(state.temperature, idx)
    
    // Compute buoyancy force
    val buoyancyForce = vec3(
      0.0f,
      params.buoyancy * (temp - params.ambient),
      0.0f
    )
    val newVel = oldVel + buoyancyForce * params.dt
    
    // Write result
    GIO.write(state.velocity, idx, newVel)
  
  @main
  def testForcesDirect(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 70)
      println("Forces Program Direct Test (No Bounds Checking)")
      println("=" * 70)
      println()
      
      val gridSize = 8
      val totalCells = gridSize * gridSize * gridSize
      
      val dt = 0.1f
      val buoyancy = 1.0f
      val ambient = 0.0f
      
      println(s"Grid: ${gridSize}³ = $totalCells cells")
      println(s"Buoyancy: $buoyancy, dt: $dt")
      println(s"Expected velocity change: ${buoyancy * (1.0f - ambient) * dt} for hot cells")
      println()
      
      val region = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          forcesDirectProgram.execute(totalCells, layout)
      
      // Initialize: zero velocity, hot spot in center
      val velocityData = Array.fill(totalCells * 3)(0.0f)
      val temperatureData = Array.ofDim[Float](totalCells)
      
      var hotCellCount = 0
      for i <- 0 until totalCells do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val cx = gridSize / 2.0
        val cy = gridSize / 2.0
        val cz = gridSize / 2.0
        val dist = Math.sqrt(Math.pow(x - cx, 2) + Math.pow(y - cy, 2) + Math.pow(z - cz, 2))
        if dist < 1.5 then
          temperatureData(i) = 1.0f
          hotCellCount += 1
        else
          temperatureData(i) = 0.0f
      
      println(s"Hot cells: $hotCellCount")
      
      val velBuffer = BufferUtils.createByteBuffer(velocityData.length * 4)
      velBuffer.asFloatBuffer().put(velocityData).flip()
      
      val tempBuffer = BufferUtils.createByteBuffer(temperatureData.length * 4)
      tempBuffer.asFloatBuffer().put(temperatureData).flip()
      
      val paramsBuffer = BufferUtils.createByteBuffer(32)
      paramsBuffer.putFloat(dt)
      paramsBuffer.putFloat(0.0001f)  // viscosity
      paramsBuffer.putFloat(0.00001f) // diffusion
      paramsBuffer.putFloat(buoyancy)
      paramsBuffer.putFloat(ambient)
      paramsBuffer.putInt(gridSize)
      paramsBuffer.putInt(2)  // iterations
      paramsBuffer.flip()
      
      val velResultBuffer = BufferUtils.createFloatBuffer(totalCells * 3)
      val velResultBB = MemoryUtil.memByteBuffer(velResultBuffer)
      
      region.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec3[Float32]](velBuffer),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](tempBuffer),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.velocity.read(velResultBB)
      )
      
      // Analyze results
      var hotCellsWithUpVel = 0
      var maxUpVel = 0.0f
      var avgUpVel = 0.0f
      var coldCellsWithVel = 0
      
      for i <- 0 until totalCells do
        val vx = velResultBuffer.get(i * 3 + 0)
        val vy = velResultBuffer.get(i * 3 + 1)
        val vz = velResultBuffer.get(i * 3 + 2)
        val temp = temperatureData(i)
        
        if temp > 0.5f then
          avgUpVel += vy
          if vy > 0.001f then
            hotCellsWithUpVel += 1
          if vy > maxUpVel then
            maxUpVel = vy
        else if Math.abs(vx) > 0.001f || Math.abs(vy) > 0.001f || Math.abs(vz) > 0.001f then
          coldCellsWithVel += 1
      
      avgUpVel /= hotCellCount
      
      println()
      println("RESULTS:")
      println(s"  Hot cells with upward velocity: $hotCellsWithUpVel / $hotCellCount")
      println(s"  Max upward velocity: $maxUpVel")
      println(s"  Average upward velocity (hot cells): $avgUpVel")
      println(s"  Cold cells with any velocity: $coldCellsWithVel")
      println()
      
      if hotCellsWithUpVel > 0 && maxUpVel > 0.05f then
        println("✅ SUCCESS! Buoyancy force is working correctly!")
        println()
        println("Sample hot cells:")
        var count = 0
        for i <- 0 until totalCells if count < 5 && temperatureData(i) > 0.5f do
          val vy = velResultBuffer.get(i * 3 + 1)
          println(s"  Cell $i: temp=${temperatureData(i)}, vy=$vy")
          count += 1
      else
        println("❌ FAILED: Buoyancy force not detected")
        println()
        println("Debug - first 10 hot cells:")
        var count = 0
        for i <- 0 until totalCells if count < 10 && temperatureData(i) > 0.5f do
          val vx = velResultBuffer.get(i * 3 + 0)
          val vy = velResultBuffer.get(i * 3 + 1)
          val vz = velResultBuffer.get(i * 3 + 2)
          println(s"  Cell $i: temp=${temperatureData(i)}, vel=($vx, $vy, $vz)")
          count += 1
      
      println("=" * 70)
      
    finally
      runtime.close()

