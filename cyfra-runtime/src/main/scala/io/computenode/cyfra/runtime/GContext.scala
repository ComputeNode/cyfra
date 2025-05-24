package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.dsl.{GArray, GStruct, GStructSchema, UniformContext, Value}
import Value.{Float32, Vec4}
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.{Binding, ComputePipeline, InputBufferSize, LayoutInfo, LayoutSet, Shader, UniformSize}
import io.computenode.cyfra.vulkan.executor.{BufferAction, SequenceExecutor}
import SequenceExecutor.*
import io.computenode.cyfra.runtime.SpirvOptimizer.{Disable, EnableOptimization}
import io.computenode.cyfra.runtime.SpirvValidator.{Enable, EnableValidation}
import io.computenode.cyfra.runtime.mem.GMem.totalStride
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.{UniformStructRef, WorkerIndex}
import io.computenode.cyfra.utility.Logger.logger
import mem.{FloatMem, GMem, Vec4FloatMem}
import org.lwjgl.system.Configuration
import izumi.reflect.Tag

import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}


class GContext(enableSpirvValidation: EnableValidation = Enable(), enableOptimization: EnableOptimization = Disable):

  Configuration.STACK_SIZE.set(1024) // fix lwjgl stack size

  val vkContext = new VulkanContext(enableValidationLayers = true)

  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))

  def compile[
    G <: GStruct[G] : {Tag, GStructSchema},
    H <: Value : {Tag, FromExpr},
    R <: Value : {Tag, FromExpr}
  ](function: GFunction[G, H, R]): ComputePipeline = {
    val uniformStructSchema = summon[GStructSchema[G]]
    val uniformStruct = uniformStructSchema.fromTree(UniformStructRef)
    val tree = function
      .fn
      .apply(
        uniformStruct,
        WorkerIndex,
        GArray[H](0)
      )
    val shaderCode = DSLCompiler.compile(tree, function.arrayInputs, function.arrayOutputs, uniformStructSchema)
    SpirvValidator.validateSpirv(shaderCode, enableSpirvValidation)

    dumpSpvToFile(shaderCode, "program.spv") // TODO remove before release

    val inOut = 0 to 1 map (Binding(_, InputBufferSize(typeStride(summon[Tag[H]]))))
    val uniform = Option.when(uniformStructSchema.fields.nonEmpty)(Binding(2, UniformSize(totalStride(uniformStructSchema))))
    val layoutInfo = LayoutInfo(Seq(LayoutSet(0, inOut ++ uniform)))

    val shader = SpirvOptimizer.getOptimizedSpirv(shaderCode, enableOptimization) match {
      case None =>
        new Shader(shaderCode, new org.joml.Vector3i(256, 1, 1), layoutInfo, "main", vkContext.device)

      case Some(optimizedShaderCode) =>
        dumpSpvToFile(optimizedShaderCode, "optimized_program.spv")
        SpirvValidator.validateSpirv(optimizedShaderCode, enableSpirvValidation)
        new Shader(optimizedShaderCode, new org.joml.Vector3i(256, 1, 1), layoutInfo, "main", vkContext.device)
    }

    new ComputePipeline(shader, vkContext)
  }

  def time[R](block: => R): R = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    logger.debug(s"Elapsed time: ${(end - start) / 1e6} ms")
    result
  }

  private def dumpSpvToFile(code: ByteBuffer, path: String): Unit =
    val fc: FileChannel = new FileOutputStream("program.spv").getChannel
    fc.write(code)
    fc.close()
    code.rewind()

  def execute[
    G <: GStruct[G] : Tag : GStructSchema,
    H <: Value,
    R <: Value
  ](mem: GMem[H], fn: GFunction[?, H, R])(using uniformContext: UniformContext[_]): GMem[R] =
    val isUniformEmpty = uniformContext.uniform.schema.fields.isEmpty
    val actions = Map(
      LayoutLocation(0, 0) -> BufferAction.LoadTo,
      LayoutLocation(0, 1) -> BufferAction.LoadFrom
    ) ++ (
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
      case t if t == Tag[Vec4[Float32]] =>
        new Vec4FloatMem(mem.size, out.head).asInstanceOf[GMem[R]]
      case _ => assert(false, "Supported output types are Float32 and Vec4[Float32]")

