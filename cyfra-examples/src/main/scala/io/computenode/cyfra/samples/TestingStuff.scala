package io.computenode.cyfra.samples

import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.memory.Variable
import io.computenode.cyfra.dsl.{*, given}
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

  case class EmitProgramUniform(emitN: UInt32)

  case class EmitProgramLayout(
    in: GBuffer[GArray[UInt32]],
    out: GBuffer[GArray[UInt32]],
    args: GUniform[EmitProgramUniform] = GUniform.fromParams, // todo will be different in the future
  )

  val emitProgram = GioProgram[EmitProgramParams, EmitProgramLayout](
    layout = params =>
      EmitProgramLayout(
        in = GBuffer(params.inSize),
        out = GBuffer(params.inSize * params.emitN),
        args = GUniform(EmitProgramUniform(UInt32(params.emitN))),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val emitN = GIO.read(layout.args).emitN
    val invocId = GIO.read(GlobalInvocationId.focus(_.x))
    val element = GIO.read(layout.in.focus(_.at(invocId)))
    val bufferOffset = invocId * emitN
    val iV: Variable[UInt32] = GIO.declare(Some(UInt32(0)))
    GIO.loop(
      {
        val i = GIO.read(iV)
        GIO.conditionalBreak(i === emitN)
        GIO.write(layout.out.focus(_.at(bufferOffset + i)), element)
      },
      GIO.write(iV, GIO.read(iV) + 1),
    )

  // === Filter program ===

  case class FilterProgramParams(inSize: Int, filterValue: Int)

  case class FilterProgramUniform(filterValue: UInt32)

  case class FilterProgramLayout(
    in: GBuffer[GArray[UInt32]],
    out: GBuffer[GArray[UInt32]],
    params: GUniform[FilterProgramUniform] = GUniform.fromParams,
  )

  val filterProgram = GioProgram[FilterProgramParams, FilterProgramLayout](
    layout = params =>
      FilterProgramLayout(
        in = GBuffer(params.inSize),
        out = GBuffer(params.inSize),
        params = GUniform(FilterProgramUniform(UInt32(params.filterValue))),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val invocId = GIO.read(GlobalInvocationId.focus(_.x))
    val element = GIO.read(layout.in.focus(_.at(invocId)))
    val isMatch = element === GIO.read(layout.params).filterValue
    val a: UInt32 = when[UInt32](isMatch)(1).otherwise(0)
    GIO.write(layout.out.focus(_.at(invocId)), a)

  // === GExecution ===

  case class EmitFilterParams(inSize: Int, emitN: Int, filterValue: Int)

  case class EmitFilterLayout(inBuffer: GBuffer[GArray[UInt32]], emitBuffer: GBuffer[GArray[UInt32]], filterBuffer: GBuffer[GArray[UInt32]])

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
      init = EmitProgramLayout(in = GBuffer[GArray[UInt32]](buffer), out = GBuffer[GArray[UInt32]](data.length * 2)),
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
        inBuffer = GBuffer[GArray[UInt32]](buffer),
        emitBuffer = GBuffer[GArray[UInt32]](data.length * 2),
        filterBuffer = GBuffer[GArray[UInt32]](data.length * 2),
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
