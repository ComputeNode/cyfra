package io.computenode.cyfra.fluids.tests

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fluids.solver.BoundaryProgram
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}
import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import java.nio.{ByteBuffer, ByteOrder}

@main def testBoundaryProgram(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()

  try
    val gridSize = 16
    val totalCells = gridSize * gridSize * gridSize
    
    println(s"Testing BoundaryProgram with grid $gridSize³ = $totalCells cells")
    
    val params = FluidParams(
      dt = 0.3f,
      viscosity = 0.0001f,
      diffusion = 0.0001f,
      buoyancy = 8.0f,
      ambient = 0.0f,
      gridSize = gridSize,
      iterationCount = 20,
      windX = 0.0f,
      windZ = 0.0f
    )
    
    val paramsBuffer = {
      import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
      import io.computenode.cyfra.dsl.struct.GStruct.given
      import io.computenode.cyfra.core.GCodec
      
      val schema = summon[io.computenode.cyfra.dsl.struct.GStructSchema[FluidParams]]
      val size = totalStride(schema)
      val buffer = ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder())
      summon[GCodec[FluidParams, FluidParams]].toByteBuffer(buffer, Array(params))
      buffer.rewind()
      buffer
    }
    
    // Initialize velocity buffer with non-zero values everywhere
    val velocityBuffer = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    for i <- 0 until totalCells do
      velocityBuffer.putFloat(1.0f)  // x
      velocityBuffer.putFloat(2.0f)  // y
      velocityBuffer.putFloat(3.0f)  // z
      velocityBuffer.putFloat(0.0f)  // w
    velocityBuffer.rewind()
    
    // Execute program
    val region = GBufferRegion
      .allocate[FluidState]
      .map: region =>
        BoundaryProgram.create.execute(totalCells, region)
    
    val velocityOut = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    
    println("\n=== BoundaryProgram Results ===")
    
    try
      region.runUnsafe(
        init = FluidState(
          velocity = GBuffer(velocityBuffer),
          pressure = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          density = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          temperature = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          divergence = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          params = GUniform(paramsBuffer)
        ),
        onDone = layout =>
          layout.velocity.read(velocityOut)
      )
      
      velocityOut.rewind()
      
      // Check boundary cells (should be zero)
      val corner = 0 // x=0, y=0, z=0 (corner)
      velocityOut.position(corner * 16)
      val cornerVx = velocityOut.getFloat()
      val cornerVy = velocityOut.getFloat()
      val cornerVz = velocityOut.getFloat()
      
      val edge = 0 + gridSize * (0 + gridSize * 1) // x=0, y=0, z=1 (edge)
      velocityOut.position(edge * 16)
      val edgeVx = velocityOut.getFloat()
      val edgeVy = velocityOut.getFloat()
      val edgeVz = velocityOut.getFloat()
      
      // Check interior cell (should still be (1, 2, 3))
      val center = gridSize/2 + gridSize * (gridSize/2 + gridSize * gridSize/2)
      velocityOut.position(center * 16)
      val centerVx = velocityOut.getFloat()
      val centerVy = velocityOut.getFloat()
      val centerVz = velocityOut.getFloat()
      
      println(f"Corner (0,0,0) velocity: ($cornerVx%.2f, $cornerVy%.2f, $cornerVz%.2f)")
      println(f"Edge (0,0,1) velocity: ($edgeVx%.2f, $edgeVy%.2f, $edgeVz%.2f)")
      println(f"Center velocity: ($centerVx%.2f, $centerVy%.2f, $centerVz%.2f)")
      
      val boundaryZero = (cornerVx == 0.0f && cornerVy == 0.0f && cornerVz == 0.0f &&
                          edgeVx == 0.0f && edgeVy == 0.0f && edgeVz == 0.0f)
      val interiorPreserved = (centerVx == 1.0f && centerVy == 2.0f && centerVz == 3.0f)
      
      if boundaryZero && interiorPreserved then
        println("✅ PASS: Boundary conditions applied correctly")
      else
        println("⚠️  WARN: Boundary conditions may be incorrect")
        if !boundaryZero then println("  - Boundary cells not zeroed")
        if !interiorPreserved then println("  - Interior cells modified")
        
    catch
      case e: Exception =>
        println(s"❌ FAIL: BoundaryProgram threw exception:")
        println(s"  ${e.getMessage}")
        e.printStackTrace()
      
  finally
    runtime.close()

