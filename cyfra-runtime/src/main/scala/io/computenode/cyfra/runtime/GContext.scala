package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.{Float32, FromExpr, Int32, Vec4}
import io.computenode.cyfra.dsl.collections.GArray
import io.computenode.cyfra.dsl.struct.*
import io.computenode.cyfra.runtime.mem.GMem.totalStride
import io.computenode.cyfra.runtime.mem.{FloatMem, GMem, IntMem, Vec4FloatMem}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.{UniformStructRef, WorkerIndex}
import io.computenode.cyfra.spirvtools.SpirvToolsRunner
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.vulkan.executor.SequenceExecutor.*
import io.computenode.cyfra.vulkan.executor.{BufferAction, SequenceExecutor}
import izumi.reflect.Tag
import org.lwjgl.system.Configuration

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class GContext(spirvToolsRunner: SpirvToolsRunner = SpirvToolsRunner()):
  Configuration.STACK_SIZE.set(1024) // fix lwjgl stack size

  val vkContext = new VulkanContext()

  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))

  def compile[G <: GStruct[G]: Tag: GStructSchema, H <: Value: Tag: FromExpr, R <: Value: Tag: FromExpr](
    function: GFunction[G, H, R],
  ): ComputePipeline = {
    val uniformStructSchema = summon[GStructSchema[G]]
    val uniformStruct = uniformStructSchema.fromTree(UniformStructRef)
    val tree = function.fn
      .apply(uniformStruct, WorkerIndex, GArray[H](0))

    val optimizedShaderCode =
      spirvToolsRunner.processShaderCodeWithSpirvTools(DSLCompiler.compile(tree, function.arrayInputs, function.arrayOutputs, uniformStructSchema))

    val inOut = 0 to 1 map (Binding(_, InputBufferSize(typeStride(summon[Tag[H]]))))
    val uniform = Option.when(uniformStructSchema.fields.nonEmpty)(Binding(2, UniformSize(totalStride(uniformStructSchema))))
    val layoutInfo = LayoutInfo(Seq(LayoutSet(0, inOut ++ uniform)))

    val shader = Shader(optimizedShaderCode, org.joml.Vector3i(256, 1, 1), layoutInfo, "main", vkContext.device)
    ComputePipeline(shader, vkContext)
  }

  def execute[G <: GStruct[G]: Tag: GStructSchema, H <: Value, R <: Value](mem: GMem[H], fn: GFunction[G, H, R])(using
    uniformContext: UniformContext[G],
  ): GMem[R] =
    val isUniformEmpty = uniformContext.uniform.schema.fields.isEmpty
    val actions = Map(LayoutLocation(0, 0) -> BufferAction.LoadTo, LayoutLocation(0, 1) -> BufferAction.LoadFrom) ++
      (
        if isUniformEmpty then Map.empty
        else Map(LayoutLocation(0, 2) -> BufferAction.LoadTo)
      )
    val sequence = ComputationSequence(Seq(Compute(fn.pipeline, actions)), Seq.empty)
    val executor = new SequenceExecutor(sequence, vkContext)

    val data = mem.toReadOnlyBuffer
    val inData =
      if isUniformEmpty then Seq(data)
      else Seq(data, GMem.serializeUniform(uniformContext.uniform))
    val out = executor.execute(inData, mem.size)
    executor.destroy()

    val outTags = fn.arrayOutputs
    assert(outTags.size == 1)

    outTags.head match
      case t if t == Tag[Float32] =>
        new FloatMem(mem.size, out.head).asInstanceOf[GMem[R]]
      case t if t == Tag[Int32] =>
        new IntMem(mem.size, out.head).asInstanceOf[GMem[R]]
      case t if t == Tag[Vec4[Float32]] =>
        new Vec4FloatMem(mem.size, out.head).asInstanceOf[GMem[R]]
      case _ => assert(false, "Supported output types are Float32 and Vec4[Float32]")
