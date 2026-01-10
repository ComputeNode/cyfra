package io.computenode.cyfra.samples.examples

import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

object Example1_DoubleAndAddPipeline:
  
  case class DoubleLayout(input: GBuffer[Float32], output: GBuffer[Float32]) extends Layout
  
  val doubleProgram: GProgram[Int, DoubleLayout] = GProgram[Int, DoubleLayout](
    layout = size => DoubleLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size)
    ),
    dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
    workgroupSize = (256, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val totalElements = 256
    GIO.when(idx < totalElements):
      val value = GIO.read(layout.input, idx)
      GIO.write(layout.output, idx, value * 2.0f)
  
  case class AddParams(value: Float32) extends GStruct[AddParams]
  
  case class AddLayout(input: GBuffer[Float32], output: GBuffer[Float32], params: GUniform[AddParams]) extends Layout
  
  val addProgram: GProgram[Int, AddLayout] = GProgram[Int, AddLayout](
    layout = size => AddLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[AddParams]()
    ),
    dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
    workgroupSize = (256, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val totalElements = 256
    GIO.when(idx < totalElements):
      val value = GIO.read(layout.input, idx)
      val addValue = layout.params.read.value
      GIO.write(layout.output, idx, value + addValue)
  
  case class PipelineLayout(
    input: GBuffer[Float32],
    doubled: GBuffer[Float32],
    output: GBuffer[Float32],
    addParams: GUniform[AddParams]
  ) extends Layout
  
  val doubleAndAddPipeline: GExecution[Int, PipelineLayout, PipelineLayout] = 
    GExecution[Int, PipelineLayout]()
      .addProgram(doubleProgram)(
        size => size,
        layout => DoubleLayout(layout.input, layout.doubled)
      )
      .addProgram(addProgram)(
        size => size,
        layout => AddLayout(layout.doubled, layout.output, layout.addParams)
      )
  
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
        doubled = GBuffer[Float32](size),
        output = GBuffer[Float32](size),
        addParams = GUniform(AddParams(10.0f))
      ),
      onDone = layout => layout.output.readArray(results)
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
  
  case class MulParams(factor: Float32) extends GStruct[MulParams]
  
  case class MulLayout(
    input: GBuffer[Float32], 
    output: GBuffer[Float32],
    params: GUniform[MulParams]
  ) extends Layout
  
  val mulProgram: GProgram[Int, MulLayout] = GProgram.static[Int, MulLayout](
    layout = size => MulLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[MulParams]()
    ),
    dispatchSize = size => size
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = GIO.read(layout.input, idx)
      val factor = layout.params.read.factor
      GIO.write(layout.output, idx, value * factor)
  
  case class AddParams(addend: Float32) extends GStruct[AddParams]
  
  case class AddLayout(
    input: GBuffer[Float32], 
    output: GBuffer[Float32],
    params: GUniform[AddParams]
  ) extends Layout
  
  val addProgram: GProgram[Int, AddLayout] = GProgram.static[Int, AddLayout](
    layout = size => AddLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[AddParams]()
    ),
    dispatchSize = size => size
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = GIO.read(layout.input, idx)
      val addend = layout.params.read.addend
      GIO.write(layout.output, idx, value + addend)
  
  case class MulAddLayout(
    input: GBuffer[Float32],
    multiplied: GBuffer[Float32],
    output: GBuffer[Float32],
    mulParams: GUniform[MulParams],
    addParams: GUniform[AddParams]
  ) extends Layout
  
  val mulAddPipeline: GExecution[Int, MulAddLayout, MulAddLayout] =
    GExecution[Int, MulAddLayout]()
      .addProgram(mulProgram)(
        size => size,
        layout => MulLayout(layout.input, layout.multiplied, layout.mulParams)
      )
      .addProgram(addProgram)(
        size => size,
        layout => AddLayout(layout.multiplied, layout.output, layout.addParams)
      )
  
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
        multiplied = GBuffer[Float32](size),
        output = GBuffer[Float32](size),
        mulParams = GUniform(MulParams(multiplyFactor)),
        addParams = GUniform(AddParams(addValue))
      ),
      onDone = layout => layout.output.readArray(results)
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
  
  case class SquareLayout(input: GBuffer[Float32], output: GBuffer[Float32]) extends Layout
  
  val squareProgram: GProgram[Int, SquareLayout] = GProgram.static[Int, SquareLayout](
    layout = size => SquareLayout(input = GBuffer[Float32](size), output = GBuffer[Float32](size)),
    dispatchSize = size => size
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = GIO.read(layout.input, idx)
      GIO.write(layout.output, idx, value * value)
  
  case class ThresholdParams(threshold: Float32) extends GStruct[ThresholdParams]
  
  case class ThresholdLayout(
    input: GBuffer[Float32], 
    output: GBuffer[Float32],
    params: GUniform[ThresholdParams]
  ) extends Layout
  
  val thresholdProgram: GProgram[Int, ThresholdLayout] = GProgram.static[Int, ThresholdLayout](
    layout = size => ThresholdLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[ThresholdParams]()
    ),
    dispatchSize = size => size
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = GIO.read(layout.input, idx)
      val threshold = layout.params.read.threshold
      val result = when(value > threshold)(1.0f).otherwise(0.0f)
      GIO.write(layout.output, idx, result)
  
  case class MapFilterLayout(
    input: GBuffer[Float32],
    squared: GBuffer[Float32],
    output: GBuffer[Float32],
    thresholdParams: GUniform[ThresholdParams]
  ) extends Layout
  
  val mapFilterPipeline: GExecution[Int, MapFilterLayout, MapFilterLayout] =
    GExecution[Int, MapFilterLayout]()
      .addProgram(squareProgram)(
        size => size,
        layout => SquareLayout(layout.input, layout.squared)
      )
      .addProgram(thresholdProgram)(
        size => size,
        layout => ThresholdLayout(layout.squared, layout.output, layout.thresholdParams)
      )
  
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
        squared = GBuffer[Float32](size),
        output = GBuffer[Float32](size),
        thresholdParams = GUniform(ThresholdParams(thresholdValue))
      ),
      onDone = layout => layout.output.readArray(results)
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
