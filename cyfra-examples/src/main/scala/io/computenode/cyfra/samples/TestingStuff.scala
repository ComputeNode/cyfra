package io.computenode.cyfra.samples

import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.dsl.archive.Value.{GBoolean, Int32}
import io.computenode.cyfra.dsl.archive.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.archive.gio.GIO
import io.computenode.cyfra.dsl.archive.struct.GStruct
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.spirvtools.SpirvTool.ToFile
import io.computenode.cyfra.spirvtools.{SpirvCross, SpirvToolsRunner, SpirvValidator}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.CollectionConverters.given

object TestingStuff:

  // === Emit program ===

  case class EmitProgramParams(inSize: Int, emitN: Int)

  case class EmitProgramUniform(emitN: Int32) extends GStruct[EmitProgramUniform]

  case class EmitProgramLayout(
    in: GBuffer[Int32],
    out: GBuffer[Int32],
    args: GUniform[EmitProgramUniform] = GUniform.fromParams, // todo will be different in the future
  ) extends Layout

  val emitProgram = GProgram[EmitProgramParams, EmitProgramLayout](
    layout = params =>
      EmitProgramLayout(
        in = GBuffer[Int32](params.inSize),
        out = GBuffer[Int32](params.inSize * params.emitN),
        args = GUniform(EmitProgramUniform(params.emitN)),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val EmitProgramUniform(emitN) = layout.args.read
    val invocId = GIO.invocationId
    val element = GIO.read(layout.in, invocId)
    val bufferOffset = invocId * emitN
    GIO.repeat(emitN): i =>
      GIO.write(layout.out, bufferOffset + i, element)

  // === Filter program ===

  case class FilterProgramParams(inSize: Int, filterValue: Int)

  case class FilterProgramUniform(filterValue: Int32) extends GStruct[FilterProgramUniform]

  case class FilterProgramLayout(in: GBuffer[Int32], out: GBuffer[Int32], params: GUniform[FilterProgramUniform] = GUniform.fromParams) extends Layout

  val filterProgram = GProgram[FilterProgramParams, FilterProgramLayout](
    layout = params =>
      FilterProgramLayout(
        in = GBuffer[Int32](params.inSize),
        out = GBuffer[Int32](params.inSize),
        params = GUniform(FilterProgramUniform(params.filterValue)),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val invocId = GIO.invocationId
    val element = GIO.read(layout.in, invocId)
    val isMatch = element === layout.params.read.filterValue
    val a: Int32 = when[Int32](isMatch)(1).otherwise(0)
    GIO.write(layout.out, invocId, a)

  // === GExecution ===

  case class EmitFilterParams(inSize: Int, emitN: Int, filterValue: Int)

  case class EmitFilterLayout(inBuffer: GBuffer[Int32], emitBuffer: GBuffer[Int32], filterBuffer: GBuffer[Int32]) extends Layout

  case class EmitFilterResult(out: GBuffer[Int32]) extends Layout

  val emitFilterExecution = GExecution[EmitFilterParams, EmitFilterLayout]()
    .addProgram(emitProgram)(
      params => EmitProgramParams(inSize = params.inSize, emitN = params.emitN),
      layout => EmitProgramLayout(in = layout.inBuffer, out = layout.emitBuffer),
    )
    .addProgram(filterProgram)(
      params => FilterProgramParams(inSize = 2 * params.inSize, filterValue = params.filterValue),
      layout => FilterProgramLayout(in = layout.emitBuffer, out = layout.filterBuffer),
    )

  @main
  def testEmit =
    given runtime: VkCyfraRuntime =
      VkCyfraRuntime(spirvToolsRunner = SpirvToolsRunner(crossCompilation = SpirvCross.Enable(toolOutput = ToFile(Paths.get("output/optimized.glsl")))))

    val emitParams = EmitProgramParams(inSize = 1024, emitN = 2)

    val region = GBufferRegion
      .allocate[EmitProgramLayout]
      .map: region =>
        emitProgram.execute(emitParams, region)

    val data = (0 until 1024).toArray
    val buffer = BufferUtils.createByteBuffer(data.length * 4)
    buffer.asIntBuffer().put(data).flip()

    val result = BufferUtils.createIntBuffer(data.length * 2)
    val rbb = MemoryUtil.memByteBuffer(result)
    region.runUnsafe(
      init = EmitProgramLayout(in = GBuffer[Int32](buffer), out = GBuffer[Int32](data.length * 2)),
      onDone = layout => layout.out.read(rbb),
    )
    runtime.close()

    val actual = (0 until 2 * 1024).map(i => result.get(i * 1))
    val expected = (0 until 1024).flatMap(x => Seq.fill(emitParams.emitN)(x))
    expected
      .zip(actual)
      .zipWithIndex
      .foreach:
        case ((e, a), i) => assert(e == a, s"Mismatch at index $i: expected $e, got $a")

  @main
  def test =
    given runtime: VkCyfraRuntime = VkCyfraRuntime(spirvToolsRunner =
      SpirvToolsRunner(
        crossCompilation = SpirvCross.Enable(toolOutput = ToFile(Paths.get("output/optimized.glsl"))),
        validator = SpirvValidator.Disable,
      ),
    )

    val emitFilterParams = EmitFilterParams(inSize = 1024, emitN = 2, filterValue = 42)

    val region = GBufferRegion
      .allocate[EmitFilterLayout]
      .map: region =>
        emitFilterExecution.execute(emitFilterParams, region)

    val data = (0 until 1024).toArray
    val buffer = BufferUtils.createByteBuffer(data.length * 4)
    buffer.asIntBuffer().put(data).flip()

    val result = BufferUtils.createIntBuffer(data.length * 2)
    val rbb = MemoryUtil.memByteBuffer(result)
    region.runUnsafe(
      init = EmitFilterLayout(
        inBuffer = GBuffer[Int32](buffer),
        emitBuffer = GBuffer[Int32](data.length * 2),
        filterBuffer = GBuffer[Int32](data.length * 2),
      ),
      onDone = layout => layout.filterBuffer.read(rbb),
    )
    runtime.close()

    val actual = (0 until 2 * 1024).map(i => result.get(i) != 0)
    val expected = (0 until 1024).flatMap(x => Seq.fill(emitFilterParams.emitN)(x)).map(_ == emitFilterParams.filterValue)
    expected
      .zip(actual)
      .zipWithIndex
      .foreach:
        case ((e, a), i) => assert(e == a, s"Mismatch at index $i: expected $e, got $a")
    println("DONE")
