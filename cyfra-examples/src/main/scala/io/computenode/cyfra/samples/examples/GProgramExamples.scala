package io.computenode.cyfra.samples.examples

import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram, Layout}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.expression.types.Float32.given
import io.computenode.cyfra.runtime.VkCyfraRuntime

object Example1_DoubleAndAddPipeline:

  case class DoubleLayout(input: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]]) derives Layout

  val doubleProgram: GProgram[Int, DoubleLayout] = GioProgram[Int, DoubleLayout](
    layout = size => DoubleLayout(input = GBuffer[RuntimeArray[Float32]](size), output = GBuffer[RuntimeArray[Float32]](size)),
    dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
    workgroupSize = (256, 1, 1),
  ): layout =>
    val idx = GIO.read(GlobalInvocationId.focus(_.x))
    val totalElements = 256
    GIO.when(idx < totalElements):
      val value = GIO.read(layout.input.focus(_.at(idx)))
      GIO.write(layout.output.focus(_.at(idx)), value * 2.0f)

  case class AddParams(value: Float32)

  case class AddLayout(input: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]], params: GUniform[AddParams]) derives Layout

  val addProgram: GProgram[Int, AddLayout] = GioProgram[Int, AddLayout](
    layout = size => AddLayout(input = GBuffer[RuntimeArray[Float32]](size), output = GBuffer[RuntimeArray[Float32]](size), params = GUniform[AddParams]()),
    dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
    workgroupSize = (256, 1, 1),
  ): layout =>
    val idx = GIO.read(GlobalInvocationId.focus(_.x))
    val totalElements = 256
    GIO.when(idx < totalElements):
      val value = GIO.read(layout.input.focus(_.at(idx)))
      val addValue = GIO.read(layout.params).value
      GIO.write(layout.output.focus(_.at(idx)), value + addValue)

  case class PipelineLayout(input: GBuffer[RuntimeArray[Float32]], doubled: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]], addParams: GUniform[AddParams])
      derives Layout

  val doubleAndAddPipeline: GExecution[Int, PipelineLayout, PipelineLayout] =
    GExecution[Int, PipelineLayout]()
      .addProgram(doubleProgram)(size => size, layout => DoubleLayout(layout.input, layout.doubled))
      .addProgram(addProgram)(size => size, layout => AddLayout(layout.doubled, layout.output, layout.addParams))

  @main
  def runDoubleAndAddPipeline(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()

    val size = 256
    val inputData = (0 until size).map(_.toFloat).toArray
    val results = Array.ofDim[Float](size)

    val region = GBufferRegion
      .allocate[PipelineLayout]
      .map: layout =>
        doubleAndAddPipeline.execute(size, layout)

    region.runUnsafe(
      init = PipelineLayout(
        input = GBuffer(inputData),
        doubled = GBuffer[RuntimeArray[Float32]](size),
        output = GBuffer[RuntimeArray[Float32]](size),
        addParams = GUniform(AddParams(10.0f)),
      ),
      onDone = layout => layout.output.readArray(results),
    )

    println("Example 1 (Extended): Double and Add Pipeline")
    println("Pipeline: input -> double -> add 10")
    println(s"Input:  ${inputData.take(5).mkString(", ")}...")
    println(s"Output: ${results.take(5).mkString(", ")}...")

    val expected = inputData.map(x => x * 2.0f + 10.0f)
    val allCorrect = results.zip(expected).forall((r, e) => Math.abs(r - e) < 0.001f)
    println(s"Expected: ${expected.take(5).mkString(", ")}...")
    println(s"All results correct: $allCorrect")
    println()

    runtime.close()

object Example2_ConfigurableMulAddPipeline:

  case class MulParams(factor: Float32)

  case class MulLayout(input: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]], params: GUniform[MulParams]) derives Layout

  val mulProgram: GProgram[Int, MulLayout] = GioProgram[Int, MulLayout](
    layout = size => MulLayout(input = GBuffer[RuntimeArray[Float32]](size), output = GBuffer[RuntimeArray[Float32]](size), params = GUniform[MulParams]()),
    dispatch = (_, size) => StaticDispatch(((size + 127) / 128, 1, 1)),
  ): layout =>
    val idx = GIO.read(GlobalInvocationId.focus(_.x))
    GIO.when(idx < 256):
      val value = GIO.read(layout.input.focus(_.at(idx)))
      val factor = GIO.read(layout.params).factor
      GIO.write(layout.output.focus(_.at(idx)), value * factor)

  case class AddParams(addend: Float32)

  case class AddLayout(input: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]], params: GUniform[AddParams]) derives Layout

  val addProgram: GProgram[Int, AddLayout] = GioProgram[Int, AddLayout](
    layout = size => AddLayout(input = GBuffer[RuntimeArray[Float32]](size), output = GBuffer[RuntimeArray[Float32]](size), params = GUniform[AddParams]()),
    dispatch = (_, size) => StaticDispatch(((size + 127) / 128, 1, 1)),
  ): layout =>
    val idx = GIO.read(GlobalInvocationId.focus(_.x))
    GIO.when(idx < 256):
      val value = GIO.read(layout.input.focus(_.at(idx)))
      val addend = GIO.read(layout.params).addend
      GIO.write(layout.output.focus(_.at(idx)), value + addend)

  case class MulAddLayout(
    input: GBuffer[RuntimeArray[Float32]],
    multiplied: GBuffer[RuntimeArray[Float32]],
    output: GBuffer[RuntimeArray[Float32]],
    mulParams: GUniform[MulParams],
    addParams: GUniform[AddParams],
  ) derives Layout

  val mulAddPipeline: GExecution[Int, MulAddLayout, MulAddLayout] =
    GExecution[Int, MulAddLayout]()
      .addProgram(mulProgram)(size => size, layout => MulLayout(layout.input, layout.multiplied, layout.mulParams))
      .addProgram(addProgram)(size => size, layout => AddLayout(layout.multiplied, layout.output, layout.addParams))

  @main
  def runConfigurableMulAddPipeline(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()

    val size = 256
    val inputData = (0 until size).map(_.toFloat).toArray
    val multiplyFactor = 3.0f
    val addValue = 100.0f
    val results = Array.ofDim[Float](size)

    val region = GBufferRegion
      .allocate[MulAddLayout]
      .map: layout =>
        mulAddPipeline.execute(size, layout)

    region.runUnsafe(
      init = MulAddLayout(
        input = GBuffer(inputData),
        multiplied = GBuffer[RuntimeArray[Float32]](size),
        output = GBuffer[RuntimeArray[Float32]](size),
        mulParams = GUniform(MulParams(multiplyFactor)),
        addParams = GUniform(AddParams(addValue)),
      ),
      onDone = layout => layout.output.readArray(results),
    )

    println("Example 2: Configurable Multiply-Add Pipeline")
    println(s"Pipeline: input -> multiply by $multiplyFactor -> add $addValue")
    println(s"Input:  ${inputData.take(5).mkString(", ")}...")
    println(s"Output: ${results.take(5).mkString(", ")}...")

    val expected = inputData.map(x => x * multiplyFactor + addValue)
    val allCorrect = results.zip(expected).forall((r, e) => Math.abs(r - e) < 0.001f)
    println(s"Expected: ${expected.take(5).mkString(", ")}...")
    println(s"All results correct: $allCorrect")
    println()

    runtime.close()

object Example4_MapFilterPipeline:

  case class SquareLayout(input: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]]) derives Layout

  val squareProgram: GProgram[Int, SquareLayout] = GioProgram[Int, SquareLayout](
    layout = size => SquareLayout(input = GBuffer[RuntimeArray[Float32]](size), output = GBuffer[RuntimeArray[Float32]](size)),
    dispatch = (_, size) => StaticDispatch(((size + 127) / 128, 1, 1)),
  ): layout =>
    val idx = GIO.read(GlobalInvocationId.focus(_.x))
    GIO.when(idx < 256):
      val value = GIO.read(layout.input.focus(_.at(idx)))
      GIO.write(layout.output.focus(_.at(idx)), value * value)

  case class ThresholdParams(threshold: Float32)

  case class ThresholdLayout(input: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]], params: GUniform[ThresholdParams]) derives Layout

  val thresholdProgram: GProgram[Int, ThresholdLayout] = GioProgram[Int, ThresholdLayout](
    layout = size => ThresholdLayout(input = GBuffer[RuntimeArray[Float32]](size), output = GBuffer[RuntimeArray[Float32]](size), params = GUniform[ThresholdParams]()),
    dispatch = (_, size) => StaticDispatch(((size + 127) / 128, 1, 1)),
  ): layout =>
    val idx = GIO.read(GlobalInvocationId.focus(_.x))
    GIO.when(idx < 256):
      val value = GIO.read(layout.input.focus(_.at(idx)))
      val threshold = GIO.read(layout.params).threshold
      val result = when[Float32](value > threshold)(1.0f).otherwise(0.0f)
      GIO.write(layout.output.focus(_.at(idx)), result)

  case class MapFilterLayout(input: GBuffer[RuntimeArray[Float32]], squared: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]], thresholdParams: GUniform[ThresholdParams])
      derives Layout

  val mapFilterPipeline: GExecution[Int, MapFilterLayout, MapFilterLayout] =
    GExecution[Int, MapFilterLayout]()
      .addProgram(squareProgram)(size => size, layout => SquareLayout(layout.input, layout.squared))
      .addProgram(thresholdProgram)(size => size, layout => ThresholdLayout(layout.squared, layout.output, layout.thresholdParams))

  @main
  def runMapFilterPipeline(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()

    val size = 256
    val inputData = (0 until size).map(_.toFloat).toArray
    val results = Array.ofDim[Float](size)
    val thresholdValue = 100.0f

    val region = GBufferRegion
      .allocate[MapFilterLayout]
      .map: layout =>
        mapFilterPipeline.execute(size, layout)

    region.runUnsafe(
      init = MapFilterLayout(
        input = GBuffer(inputData),
        squared = GBuffer[RuntimeArray[Float32]](size),
        output = GBuffer[RuntimeArray[Float32]](size),
        thresholdParams = GUniform(ThresholdParams(thresholdValue)),
      ),
      onDone = layout => layout.output.readArray(results),
    )

    println("Example 4: Map-Filter Pipeline")
    println("Pipeline: square -> threshold (x² > 100)")
    println(s"Input:  ${inputData.slice(8, 14).mkString(", ")}  (values 8-13)")
    println(s"Output: ${results.slice(8, 14).mkString(", ")}  (1.0 if squared > 100)")

    val passedCount = results.count(_ > 0.5f)
    println(s"Values that passed (x² > 100, so x > 10): $passedCount")

    val expected = inputData.map(x => if x * x > 100.0f then 1.0f else 0.0f)
    val allCorrect = results.zip(expected).forall((r, e) => Math.abs(r - e) < 0.001f)
    println(s"All results correct: $allCorrect")
    println()

    runtime.close()

object RunAllGProgramExamples:
  @main
  def runAll(): Unit =
    Example1_DoubleAndAddPipeline.runDoubleAndAddPipeline()
    Example2_ConfigurableMulAddPipeline.runConfigurableMulAddPipeline()
    Example4_MapFilterPipeline.runMapFilterPipeline()
    println("All GProgram examples completed!")
