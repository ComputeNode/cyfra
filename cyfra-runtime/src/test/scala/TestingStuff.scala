import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.ops.*
import io.computenode.cyfra.core.expression.ops.given
import io.computenode.cyfra.dsl.Library.*
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.binding.*
import io.computenode.cyfra.dsl.direct.*
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.spirvtools.SpirvTool.ToFile
import io.computenode.cyfra.spirvtools.{SpirvCross, SpirvToolsRunner, SpirvValidator}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

import java.nio.file.Paths

object TestingStuff:

  // === Emit program ===

  case class EmitProgramParams(inSize: Int, emitN: Int)

  type EmitProgramUniform = UInt32

  case class EmitProgramLayout(
    in: GBuffer[UInt32],
    out: GBuffer[UInt32],
    args: GUniform[EmitProgramUniform] = GUniform.fromParams, // todo will be different in the future
  )

  val emitProgram = GioProgram[EmitProgramParams, EmitProgramLayout](
    layout = params =>
      EmitProgramLayout(in = GBuffer[UInt32](params.inSize), out = GBuffer[UInt32](params.inSize * params.emitN), args = GUniform(UInt32(params.emitN))),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val invocId = invocationId
    val emitN = GIO.read(layout.args)
    val element = GIO.read(layout.in, invocId)
    val bufferOffset = invocId * emitN
    val iV: Var[UInt32] = GIO.declare()
    GIO.write(iV, UInt32(0))
    val body: GIO ?=> Unit =
      val i = GIO.read(iV)
      GIO.write(layout.out, bufferOffset + i, element)

    val continue: GIO ?=> Unit =
      val i = GIO.read(iV)
      GIO.write(iV, i + UInt32(1))

    GIO.loop(body, continue)

  // === Filter program ===

  case class FilterProgramParams(inSize: Int, filterValue: Int)

  type FilterProgramUniform = UInt32

  case class FilterProgramLayout(in: GBuffer[UInt32], out: GBuffer[UInt32], params: GUniform[FilterProgramUniform] = GUniform.fromParams)

  val filterProgram = GioProgram[FilterProgramParams, FilterProgramLayout](
    layout = params => FilterProgramLayout(in = GBuffer(params.inSize), out = GBuffer(params.inSize), params = GUniform(UInt32(params.filterValue))),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val invocId = invocationId
    val element = GIO.read(layout.in, invocId)
    val isMatch = element === GIO.read(layout.params)
    val a = when(isMatch)(UInt32(1))(UInt32(0))
    GIO.write(layout.out, invocId, a)

  // === GExecution ===

  case class EmitFilterParams(inSize: Int, emitN: Int, filterValue: Int)

  case class EmitFilterLayout(inBuffer: GBuffer[UInt32], emitBuffer: GBuffer[UInt32], filterBuffer: GBuffer[UInt32])

  case class EmitFilterResult(out: GBuffer[UInt32])

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
    region.runUnsafe(init = EmitProgramLayout(in = GBuffer(buffer), out = GBuffer(data.length * 2)), onDone = layout => layout.out.read(rbb))
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
      init = EmitFilterLayout(inBuffer = GBuffer(buffer), emitBuffer = GBuffer(data.length * 2), filterBuffer = GBuffer(data.length * 2)),
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
