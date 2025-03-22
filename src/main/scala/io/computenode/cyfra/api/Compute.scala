package io.computenode.cyfra.api

import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.*
import org.lwjgl.system.MemoryUtil
import io.computenode.cyfra.dsl.Algebra.*
import io.computenode.cyfra.dsl.{derived, Empty, GFunction, GArray2DFunction}
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Algebra.given
import io.computenode.cyfra.dsl.Control.*

object Compute:

  /** Map method applies a GPU-based transformation to an array of Float values */
  // transform: Float => Float: The transformation function that defines how each element in the input array is modified.
  // ctx: GContext: The context in which the GPU computation will be executed.
  def map(input: Array[Float], transform: Float => Float)(using ctx: GContext): Array[Float] =
    require(input != null && input.nonEmpty, "Input array cannot be null or empty")
    println("[Compute] Starting map operation.")
    val mem = FloatMem(input)
    val gpuFn = GFunction { (x: Float32) =>
      x.tree match
        case ConstFloat32(value) => Float32(ConstFloat32(transform(value)))
        case _ => throw new IllegalArgumentException("Non-constant Float32 encountered during map operation.")
    }
    // Execute and retrieve results
    val future = mem.map(gpuFn)
    val result = scala.concurrent.Await.result(future, scala.concurrent.duration.Duration.Inf)
    println("[Compute] Map operation completed.")
    result

  /** process2D method processes 2D data using GPU-based transformations. It computes a value for each cell in a 2D grid based on its coordinates. */
  // f: (Float, Float, Float) => Float: The transformation function that defines how each cell in the grid is computed based on its coordinates.
  def process2D(width: Int, height: Int, f: (Float, Float, Float) => Float)(using ctx: GContext): Array[Float] =
    require(width > 0 && height > 0, s"Invalid dimensions: $width x $height")
    println(s"[Compute] Starting 2D processing for $width x $height.")
    val data = Array.fill(width * height)(0.0f)
    val mem = FloatMem(data)
    val func = GArray2DFunction[Empty, Float32, Float32](width, height, { (_, coords, _) =>
      val (xi, yi) = coords
      (xi.tree, yi.tree) match
        case (ConstInt32(x), ConstInt32(y)) =>
          val input = (x.toFloat + y.toFloat) / math.max(width, height)
          Float32(ConstFloat32(f(x.toFloat, y.toFloat, input)))
        case _ =>
          throw new IllegalArgumentException("Non-constant Int32 encountered during 2D processing.")
    })
    // Execute and retrieve results
    val future = mem.map(func)
    val result = scala.concurrent.Await.result(future, scala.concurrent.duration.Duration.Inf)
    println("[Compute] 2D processing completed.")
    result