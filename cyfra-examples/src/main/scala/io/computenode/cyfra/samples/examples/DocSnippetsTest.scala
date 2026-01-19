package io.computenode.cyfra.samples.examples

import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

/**
 * Test snippets from gpu-programs.md documentation
 */
object DocSnippets_GpuPrograms:

  // === Snippet 1: Basic DoubleLayout and doubleProgram ===
  case class DoubleLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32]
  ) derives Layout

  val doubleProgram: GProgram[Int, DoubleLayout] = GProgram.static[Int, DoubleLayout](
    layout = size => DoubleLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size)
    ),
    dispatchSize = size => size,
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = layout.input.read(idx)
      layout.output.write(idx, value * 2.0f)

  @main
  def testDoubleProgram(): Unit = VkCyfraRuntime.using:
    val size = 256
    val inputData = (0 until size).map(_.toFloat).toArray
    val results = Array.ofDim[Float](size)

    val region = GBufferRegion
      .allocate[DoubleLayout]
      .map: layout =>
        doubleProgram.execute(size, layout)

    region.runUnsafe(
      init = DoubleLayout(
        input = GBuffer(inputData),
        output = GBuffer[Float32](size),
      ),
      onDone = layout => layout.output.readArray(results),
    )

    println(s"[testDoubleProgram] Results: ${results.take(5).mkString(", ")}...")
    val expected = inputData.map(_ * 2.0f)
    val correct = results.zip(expected).forall((r, e) => Math.abs(r - e) < 0.001f)
    println(s"[testDoubleProgram] Correct: $correct")

  // === Snippet 2: Parameterized Program with MulParams ===
  case class MulParams(factor: Float32) extends GStruct[MulParams]

  case class MulLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32],
    params: GUniform[MulParams]
  ) derives Layout

  val mulProgram: GProgram[Int, MulLayout] = GProgram.static[Int, MulLayout](
    layout = size => MulLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[MulParams]()
    ),
    dispatchSize = size => size,
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = layout.input.read(idx)
      val factor = layout.params.read.factor
      layout.output.write(idx, value * factor)

  @main
  def testMulProgram(): Unit = VkCyfraRuntime.using:
    val size = 256
    val inputData = (0 until size).map(_.toFloat).toArray
    val results = Array.ofDim[Float](size)

    val region = GBufferRegion
      .allocate[MulLayout]
      .map: layout =>
        mulProgram.execute(size, layout)

    region.runUnsafe(
      init = MulLayout(
        input = GBuffer(inputData),
        output = GBuffer[Float32](size),
        params = GUniform(MulParams(3.0f)),
      ),
      onDone = layout => layout.output.readArray(results),
    )

    println(s"[testMulProgram] Input:  ${inputData.take(5).mkString(", ")}...")
    println(s"[testMulProgram] Output: ${results.take(5).mkString(", ")}...")
    val expected = inputData.map(_ * 3.0f)
    val correct = results.zip(expected).forall((r, e) => Math.abs(r - e) < 0.001f)
    println(s"[testMulProgram] Correct: $correct")

  // === Snippet 3: Running Multiple Programs ===
  case class AddParams(addend: Float32) extends GStruct[AddParams]
  case class AddLayout(input: GBuffer[Float32], output: GBuffer[Float32], params: GUniform[AddParams]) derives Layout

  val addProgram: GProgram[Int, AddLayout] = GProgram.static[Int, AddLayout](
    layout = size => AddLayout(GBuffer[Float32](size), GBuffer[Float32](size), GUniform[AddParams]()),
    dispatchSize = size => size
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = layout.input.read(idx)
      val addend = layout.params.read.addend
      layout.output.write(idx, value + addend)

  case class PipelineLayout(
    input: GBuffer[Float32],
    intermediate: GBuffer[Float32],
    output: GBuffer[Float32],
    addParams: GUniform[AddParams]
  ) derives Layout

  @main
  def testMultiplePrograms(): Unit = VkCyfraRuntime.using:
    val size = 256
    val inputData = (0 until size).map(_.toFloat).toArray
    val results = Array.ofDim[Float](size)

    val region = GBufferRegion
      .allocate[PipelineLayout]
      .map: layout =>
        // Program 1: input -> intermediate (doubles values)
        val afterDouble = doubleProgram.execute(size, DoubleLayout(layout.input, layout.intermediate))

        // Program 2: use intermediate from afterDouble as input, write to output
        addProgram.execute(size, AddLayout(afterDouble.output, layout.output, layout.addParams))
        
        // Return the original layout so onDone can access it
        layout

    region.runUnsafe(
      init = PipelineLayout(
        input = GBuffer(inputData),
        intermediate = GBuffer[Float32](size),
        output = GBuffer[Float32](size),
        addParams = GUniform(AddParams(10.0f)),
      ),
      onDone = layout => layout.output.readArray(results),
    )

    println(s"[testMultiplePrograms] Pipeline: input -> double -> add 10")
    println(s"[testMultiplePrograms] Input:  ${inputData.take(5).mkString(", ")}...")
    println(s"[testMultiplePrograms] Output: ${results.take(5).mkString(", ")}...")
    val expected = inputData.map(x => x * 2.0f + 10.0f)
    val correct = results.zip(expected).forall((r, e) => Math.abs(r - e) < 0.001f)
    println(s"[testMultiplePrograms] Correct: $correct")


/**
 * Test snippets from gpu-pipelines.md documentation
 */
object DocSnippets_GpuPipelines:

  case class DoubleLayout(input: GBuffer[Float32], output: GBuffer[Float32]) derives Layout

  val doubleProgram: GProgram[Int, DoubleLayout] = GProgram[Int, DoubleLayout](
    layout = size => DoubleLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size)
    ),
    dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
    workgroupSize = (256, 1, 1),
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = layout.input.read(idx)
      layout.output.write(idx, value * 2.0f)

  case class AddParams(value: Float32) extends GStruct[AddParams]
  case class AddLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32],
    params: GUniform[AddParams]
  ) derives Layout

  val addProgram: GProgram[Int, AddLayout] = GProgram[Int, AddLayout](
    layout = size => AddLayout(
      input = GBuffer[Float32](size),
      output = GBuffer[Float32](size),
      params = GUniform[AddParams]()
    ),
    dispatch = (_, size) => StaticDispatch(((size + 255) / 256, 1, 1)),
    workgroupSize = (256, 1, 1),
  ): layout =>
    val idx = GIO.invocationId
    GIO.when(idx < 256):
      val value = layout.input.read(idx)
      val addValue = layout.params.read.value
      layout.output.write(idx, value + addValue)

  case class PipelineLayout(
    input: GBuffer[Float32],
    doubled: GBuffer[Float32],
    output: GBuffer[Float32],
    addParams: GUniform[AddParams]
  ) derives Layout

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
  def testGExecutionPipeline(): Unit = VkCyfraRuntime.using:
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
        addParams = GUniform(AddParams(10.0f)),
      ),
      onDone = layout => layout.output.readArray(results),
    )

    println(s"[testGExecutionPipeline] Pipeline: input -> double -> add 10")
    println(s"[testGExecutionPipeline] Input:  ${inputData.take(5).mkString(", ")}...")
    println(s"[testGExecutionPipeline] Output: ${results.take(5).mkString(", ")}...")
    val expected = inputData.map(x => x * 2.0f + 10.0f)
    val correct = results.zip(expected).forall((r, e) => Math.abs(r - e) < 0.001f)
    println(s"[testGExecutionPipeline] Correct: $correct")


/**
 * Run all documentation snippet tests
 */
object RunAllDocSnippetTests:
  @main
  def runAllDocSnippets(): Unit =
    println("=== Testing Documentation Snippets ===\n")

    println("--- gpu-programs.md snippets ---")
    DocSnippets_GpuPrograms.testDoubleProgram()
    println()
    DocSnippets_GpuPrograms.testMulProgram()
    println()
    DocSnippets_GpuPrograms.testMultiplePrograms()
    println()

    println("--- gpu-pipelines.md snippets ---")
    DocSnippets_GpuPipelines.testGExecutionPipeline()
    println()

    println("=== All documentation snippet tests completed! ===")
