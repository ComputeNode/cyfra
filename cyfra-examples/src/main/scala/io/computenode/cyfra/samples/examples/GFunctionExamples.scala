package io.computenode.cyfra.samples.examples

import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.{GFunction, ImageUtility}
import io.computenode.cyfra.runtime.VkCyfraRuntime

import java.nio.file.Paths

object GFunctionExamples:

  private lazy val runtime: VkCyfraRuntime = VkCyfraRuntime()
  given CyfraRuntime = runtime

  def example1_HelloGpu(): Unit =
    val doubleIt: GFunction[GStruct.Empty, Float32, Float32] = GFunction: x =>
      x * 2.0f

    val input = (0 until 256).map(_.toFloat).toArray
    val result: Array[Float] = doubleIt.run(input)

    println("Example 1: Hello GPU - Array Multiplication")
    println(s"Input:  ${input.take(10).mkString(", ")}...")
    println(s"Output: ${result.take(10).mkString(", ")}...")

    val allCorrect = input.zip(result).forall((in, out) => Math.abs(out - in * 2.0f) < 0.001f)
    println(s"All results correct: $allCorrect")
    println()

  def example2_VectorOperations(): Unit =
    val normalizeVec4: GFunction[GStruct.Empty, Vec4[Float32], Vec4[Float32]] = GFunction: v =>
      normalize(v)

    val dotWithX: GFunction[GStruct.Empty, Vec4[Float32], Float32] = GFunction: v =>
      val xAxis = vec4(1.0f, 0.0f, 0.0f, 0.0f)
      v.dot(xAxis)

    val vectors: Array[fRGBA] = Array((3.0f, 0.0f, 0.0f, 0.0f), (0.0f, 4.0f, 0.0f, 0.0f), (1.0f, 1.0f, 1.0f, 1.0f), (5.0f, 0.0f, 0.0f, 0.0f)) ++
      Array.fill(252)((1.0f, 2.0f, 3.0f, 4.0f))

    val normalized: Array[fRGBA] = normalizeVec4.run(vectors)

    println("Example 2: Vector Operations")
    println("Normalize test (Vec4):")
    println(f"  (3,0,0,0) -> (${normalized(0)._1}%.3f, ${normalized(0)._2}%.3f, ${normalized(0)._3}%.3f, ${normalized(0)._4}%.3f)")
    println(f"  (0,4,0,0) -> (${normalized(1)._1}%.3f, ${normalized(1)._2}%.3f, ${normalized(1)._3}%.3f, ${normalized(1)._4}%.3f)")
    println(f"  (1,1,1,1) -> (${normalized(2)._1}%.3f, ${normalized(2)._2}%.3f, ${normalized(2)._3}%.3f, ${normalized(2)._4}%.3f)")

    val vec4s: Array[fRGBA] = Array((1.0f, 0.0f, 0.0f, 0.0f), (5.0f, 3.0f, 2.0f, 1.0f), (-2.0f, 1.0f, 1.0f, 1.0f)) ++
      Array.fill(253)((0.0f, 0.0f, 0.0f, 0.0f))

    val dots: Array[Float] = dotWithX.run(vec4s)

    println("Dot product with X-axis (1,0,0,0):")
    println(s"  (1,0,0,0) · X = ${dots(0)}")
    println(s"  (5,3,2,1) · X = ${dots(1)}")
    println(s"  (-2,1,1,1) · X = ${dots(2)}")
    println()

  case class PhysicsConfig(gravity: Float32, dt: Float32) extends GStruct[PhysicsConfig]

  def example3_CustomStructs(): Unit =
    val applyGravity: GFunction[PhysicsConfig, Vec4[Float32], Vec4[Float32]] =
      GFunction.forEachIndex[PhysicsConfig, Vec4[Float32], Vec4[Float32]]:
        case (config, idx, buffer) =>
          val p = buffer.read(idx)
          val gravityEffect = config.gravity * config.dt * config.dt * 0.5f * p.w
          val newPosY = p.y + gravityEffect
          vec4(p.x, newPosY, p.z, p.w)

    val physics = PhysicsConfig(-9.8f, 0.1f)

    val particles: Array[fRGBA] = Array((0.0f, 100.0f, 0.0f, 1.0f), (5.0f, 200.0f, 0.0f, 2.0f), (10.0f, 50.0f, 0.0f, 0.5f)) ++
      Array.fill(253)((0.0f, 0.0f, 0.0f, 1.0f))

    val updated: Array[fRGBA] = applyGravity.run(particles, physics)

    println("Example 3: Custom Structs (Particle Simulation with Physics Config)")
    println(s"Physics: gravity=${-9.8f}, dt=0.1s")
    println("After applying gravity:")
    println(f"  Particle 1 (mass=1): Y: 100.0 -> ${updated(0)._2}%.3f")
    println(f"  Particle 2 (mass=2): Y: 200.0 -> ${updated(1)._2}%.3f")
    println(f"  Particle 3 (mass=0.5): Y: 50.0 -> ${updated(2)._2}%.3f")
    println()

  case class FractalConfig(width: Int32, height: Int32) extends GStruct[FractalConfig]

  val MaxIterations = 256

  def example6_Mandelbrot(): Unit =
    val width = 512
    val height = 512

    val mandelbrot: GFunction[FractalConfig, Int32, Vec4[Float32]] =
      GFunction.forEachIndex[FractalConfig, Int32, Vec4[Float32]]:
        case (config, idx, buffer) =>
          val pixelIdx = buffer.read(idx)
          val px = pixelIdx.mod(config.width)
          val py = pixelIdx / config.width
          val cx = px.asFloat / config.width.asFloat * 3.5f - 2.5f
          val cy = py.asFloat / config.height.asFloat * 2.4f - 1.2f

          val iterations = GSeq
            .gen(vec2(0.0f, 0.0f), z => vec2(z.x * z.x - z.y * z.y + cx, 2.0f * z.x * z.y + cy))
            .limit(MaxIterations)
            .takeWhile(z => z.x * z.x + z.y * z.y < 4.0f)
            .count

          val t = iterations.asFloat / MaxIterations.toFloat
          val r = t * t
          val g = t
          val b = sqrt(t)
          vec4(r, g, b, 1.0f)

    val indices = (0 until width * height).toArray
    val config = FractalConfig(width, height)

    println("Example 6: Mandelbrot Set")
    println(s"Computing ${width}x$height Mandelbrot set on GPU...")

    val colors: Array[fRGBA] = mandelbrot.run(indices, config)

    ImageUtility.renderToImage(colors, width, height, Paths.get("examples_output/mandelbrot.png"))
    println(s"Saved to examples_output/mandelbrot.png")
    println()

  case class JuliaConfig(width: Int32, height: Int32, cReal: Float32, cImag: Float32) extends GStruct[JuliaConfig]

  def example7_JuliaSet(): Unit =
    val width = 512
    val height = 512
    val cReal = -0.7f
    val cImag = 0.27015f

    val julia: GFunction[JuliaConfig, Int32, Vec4[Float32]] =
      GFunction.forEachIndex[JuliaConfig, Int32, Vec4[Float32]]:
        case (config, idx, buffer) =>
          val pixelIdx = buffer.read(idx)
          val px = pixelIdx.mod(config.width)
          val py = pixelIdx / config.width
          val zx = px.asFloat / config.width.asFloat * 3.0f - 1.5f
          val zy = py.asFloat / config.height.asFloat * 3.0f - 1.5f

          val iterations = GSeq
            .gen(vec2(zx, zy), z => vec2(z.x * z.x - z.y * z.y + config.cReal, 2.0f * z.x * z.y + config.cImag))
            .limit(MaxIterations)
            .takeWhile(z => z.x * z.x + z.y * z.y < 4.0f)
            .count

          val t = iterations.asFloat / MaxIterations.toFloat
          vec4(interpolate(Blue, t), 1.0f)

    val indices = (0 until width * height).toArray
    val config = JuliaConfig(width, height, cReal, cImag)

    println("Example 7: Julia Set")
    println(s"Computing ${width}x$height Julia set (c = $cReal + ${cImag}i) on GPU...")

    val colors: Array[fRGBA] = julia.run(indices, config)

    ImageUtility.renderToImage(colors, width, height, Paths.get("examples_output/julia.png"))
    println(s"Saved to examples_output/julia.png")
    println()

  case class TransformConfig(scale: Float32, offset: Float32) extends GStruct[TransformConfig]

  def example8_Uniforms(): Unit =
    val transform: GFunction[TransformConfig, Float32, Float32] =
      GFunction.forEachIndex[TransformConfig, Float32, Float32]:
        case (config, idx, buffer) =>
          buffer.read(idx) * config.scale + config.offset

    val data = (0 until 256).map(_.toFloat).toArray

    println("Example 8: Uniforms (Same program, different parameters)")
    println(s"Original data: ${data.take(5).mkString(", ")}...")

    val doubled: Array[Float] = transform.run(data, TransformConfig(2.0f, 0.0f))
    println(s"f(x) = x * 2:  ${doubled.take(5).mkString(", ")}...")

    val plusTen: Array[Float] = transform.run(data, TransformConfig(1.0f, 10.0f))
    println(s"f(x) = x + 10: ${plusTen.take(5).mkString(", ")}...")

    val scaled: Array[Float] = transform.run(data, TransformConfig(0.5f, 100.0f))
    println(s"f(x) = 0.5x + 100: ${scaled.take(5).mkString(", ")}...")

    val allCorrect = data.indices.forall { i =>
      Math.abs(doubled(i) - data(i) * 2.0f) < 0.001f && Math.abs(plusTen(i) - (data(i) + 10.0f)) < 0.001f &&
      Math.abs(scaled(i) - (data(i) * 0.5f + 100.0f)) < 0.001f
    }
    println(s"All results correct: $allCorrect")
    println()

  @main
  def runAllGFunctionExamples(): Unit =
    println("=" * 60)
    println("Running all GFunction examples")
    println("=" * 60)
    println()

    try
      example1_HelloGpu()
      example2_VectorOperations()
      example3_CustomStructs()
      example6_Mandelbrot()
      example7_JuliaSet()
      example8_Uniforms()

      println("=" * 60)
      println("All GFunction examples completed successfully!")
      println("=" * 60)
    finally runtime.close()
