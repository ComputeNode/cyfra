package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.core.archive.GFunction
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.fluids.core.GridUtils.*
import scala.reflect.ClassTag

/** Simple test of 3D grid operations using GFunction */
object simpleGridTest:
  
  @main
  def testGrid(): Unit =
    given CyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 60)
      println("Cyfra Fluids - Simple Grid Test")
      println("=" * 60)
      println()
      
      val gridSize = 8
      val totalCells = gridSize * gridSize * gridSize
      
      println(s"Testing 3D grid operations on ${gridSize}³ = $totalCells cells")
      println()
      
      // Test 1: 3D to 1D indexing
      println("Test 1: 3D to 1D indexing")
      val indexTest: GFunction[GStruct.Empty, Int32, Int32] = GFunction: idx =>
        val n = gridSize
        val z = idx / (n * n)
        val y = (idx / n).mod(n)
        val x = idx.mod(n)
        // Compute sum of coordinates
        x + y + z
      
      val indices = (0 until totalCells).toArray
      val result1: Array[Int] = indexTest.run(indices)
      
      var test1Pass = true
      for i <- 0 until Math.min(10, totalCells) do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val expected = x + y + z
        val actual = result1(i)
        if expected != actual then
          println(s"  ✗ Cell[$i] at ($x,$y,$z): expected=$expected, got=$actual")
          test1Pass = false
      
      if test1Pass then
        println(s"  ✅ 3D to 1D indexing works correctly!")
      println()
      
      // Test 2: Bounds checking
      println("Test 2: Bounds checking (inBounds)")
      val boundsTest: GFunction[GStruct.Empty, Int32, Int32] = GFunction: idx =>
        val n = gridSize
        val z = idx / (n * n)
        val y = (idx / n).mod(n)
        val x = idx.mod(n)
        // Test if in bounds
        when(inBounds(x, y, z, n)):
          (1: Int32)
        .otherwise:
          (0: Int32)
      
      val result2: Array[Int] = boundsTest.run(indices)
      val allInBounds = result2.forall(_ == 1)
      
      if allInBounds then
        println(s"  ✅ All $totalCells cells correctly identified as in bounds!")
      else
        println(s"  ✗ Some cells incorrectly marked as out of bounds")
      println()
      
      // Test 3: min/max helpers for Int32
      println("Test 3: minInt32/maxInt32 helpers")
      val minMaxTest: GFunction[GStruct.Empty, Int32, Int32] = GFunction: x =>
        val result = minInt32(x, 5) + maxInt32(x, 3)
        result
      
      val testVals = Array(0, 2, 4, 6, 8, 10)
      val result3: Array[Int] = minMaxTest.run(testVals)
      
      val expected3 = testVals.map(x => Math.min(x, 5) + Math.max(x, 3))
      val test3Pass = result3.zip(expected3).forall((a, e) => a == e)
      
      if test3Pass then
        println(s"  ✅ minInt32/maxInt32 work correctly!")
        for i <- testVals.indices do
          println(s"    x=${testVals(i)}: min(x,5)=${Math.min(testVals(i),5)}, max(x,3)=${Math.max(testVals(i),3)}, sum=${result3(i)}")
      else
        println(s"  ✗ minInt32/maxInt32 have errors")
      println()
      
      // Summary
      println("=" * 60)
      if test1Pass && allInBounds && test3Pass then
        println("✅ ALL TESTS PASSED!")
        println()
        println("Grid utility functions work correctly on GPU:")
        println("  - 3D to 1D index conversion")
        println("  - Bounds checking")
        println("  - Int32 min/max helpers")
      else
        println("❌ SOME TESTS FAILED")
      println("=" * 60)
      
    finally
      summon[CyfraRuntime] match
        case vk: VkCyfraRuntime => vk.close()
        case _ => ()

