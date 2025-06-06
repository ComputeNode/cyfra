package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.dsl.{GArray, GStruct, GStructSchema, UniformContext, Value}
import GStruct.Empty
import Value.{Float32, Vec4}
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.{Binding, ComputePipeline, InputBufferSize, LayoutInfo, LayoutSet, Shader, UniformSize}
import io.computenode.cyfra.vulkan.executor.{BufferAction, SequenceExecutor}
import SequenceExecutor.*
import io.computenode.cyfra.runtime.mem.GMem.totalStride
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler 
import io.computenode.cyfra.dsl.Expression.E
import mem.{FloatMem, GMem, Vec4FloatMem}
import org.lwjgl.system.{Configuration, MemoryUtil}
import izumi.reflect.Tag
import io.computenode.cyfra.vulkan.memory.Buffer
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.util.vma.Vma.*
import java.nio.ByteBuffer
import java.io.{FileOutputStream, IOException}
import java.nio.channels.FileChannel
import scala.collection.mutable 
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}


class GContext(debug: Boolean = false):
  val vkContext = VulkanContext(debug)
  private val pipelineCache = mutable.Map[Any, ComputePipeline]()

  private def createPipeline[G <: GStruct[G] : GStructSchema, H <: Value : Tag : FromExpr, R <: Value : Tag : FromExpr](
    function: GFunction[G, H, R]
  ): ComputePipeline = {
    val uniformStructSchemaImpl = summon[GStructSchema[G]]
    val tagGImpl: Tag[G] = uniformStructSchemaImpl.structTag 

    val uniformStruct = uniformStructSchemaImpl.fromTree(
      ExpressionCompiler.UniformStructRef[G](using tagGImpl).asInstanceOf[E[G]]
    )
    val tree = function
      .fn 
      .apply(
        uniformStruct,
        ExpressionCompiler.WorkerIndex, 
        GArray[H](0)
      )
    val shaderCode = DSLCompiler.compile(tree, function.arrayInputs, function.arrayOutputs, uniformStructSchemaImpl)
    dumpSpvToFile(shaderCode, "program.spv") // TODO remove before release

    val inputBinding = Binding(0, InputBufferSize(typeStride(summon[Tag[H]])))
    val outputBinding = Binding(1, InputBufferSize(typeStride(summon[Tag[R]])))
    
    val uniformBindingOpt = Option.when(uniformStructSchemaImpl.fields.nonEmpty)(
      Binding(2, UniformSize(GMem.totalStride(uniformStructSchemaImpl)))
    )
    
    val bindings = Seq(inputBinding, outputBinding) ++ uniformBindingOpt.toSeq
    val layoutInfo = LayoutInfo(Seq(LayoutSet(0, bindings)))
    
    val shader = new Shader(shaderCode, new org.joml.Vector3i(256, 1, 1), layoutInfo, "main", vkContext.device)
    new ComputePipeline(shader, vkContext)
  }

  private def dumpSpvToFile(code: ByteBuffer, path: String): Unit =
    try {
      val fc: FileChannel = new FileOutputStream(path).getChannel
      fc.write(code)
      fc.close()
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      code.rewind()
    }

  def execute[
    G <: GStruct[G] : Tag : GStructSchema,
    H <: Value : Tag : FromExpr, 
    R <: Value : FromExpr : Tag 
  ](mem: GMem[H], uniformStruct: G, fn: GFunction[G, H, R]): GMem[R] = {
    val pipeline = pipelineCache.getOrElseUpdate(fn.fn, createPipeline(fn))

    val sourceBuffersForExecutor = ListBuffer[Buffer]()
    val bufferActions = mutable.Map[LayoutLocation, BufferAction]()

    bufferActions.put(LayoutLocation(0, 0), BufferAction.LoadTo)
    sourceBuffersForExecutor.addOne(mem.vulkanBuffer)

    bufferActions.put(LayoutLocation(0, 1), BufferAction.LoadFrom) 

    var uniformStagingBufferOpt: Option[Buffer] = None
    val uniformStructSchema = summon[GStructSchema[G]]
    if (uniformStructSchema.fields.nonEmpty) {
      val uniformCPUByteBuffer = GMem.serializeUniform(uniformStruct)
      val uniformStagingVkBuffer = new Buffer(
        uniformCPUByteBuffer.remaining(), // Changed from .toLong to direct Int, or .toInt if remaining() can exceed Int
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        VMA_MEMORY_USAGE_CPU_ONLY,
        vkContext.allocator
      )
      uniformStagingVkBuffer.map { mappedUniform =>
        mappedUniform.put(uniformCPUByteBuffer)
      }
      
      uniformStagingBufferOpt = Some(uniformStagingVkBuffer)
      bufferActions.put(LayoutLocation(0, 2), BufferAction.LoadTo)
      sourceBuffersForExecutor.addOne(uniformStagingVkBuffer)
    }

    val computeStep = Compute(pipeline, bufferActions.toMap)
    val sequence = ComputationSequence(Seq(computeStep), dependencies = Nil) 
    val sequenceExecutor = new SequenceExecutor(sequence, vkContext) 

    val outputVulkanBuffers = sequenceExecutor.execute(sourceBuffersForExecutor.toSeq, mem.size)
    
    uniformStagingBufferOpt.foreach(_.destroy())

    if (outputVulkanBuffers.isEmpty) {
      throw new IllegalStateException("SequenceExecutor did not return an output buffer.")
    }
    val resultVulkanBuffer = outputVulkanBuffers.head

    val tagR = summon[Tag[R]]
    val resultMem = 
      if (tagR.tag =:= Tag[Float32].tag) { 
        new FloatMem(mem.size, resultVulkanBuffer).asInstanceOf[GMem[R]]
      } else if (tagR.tag =:= Tag[Vec4[Float32]].tag) { 
        new Vec4FloatMem(mem.size, resultVulkanBuffer).asInstanceOf[GMem[R]]
      } else {
        resultVulkanBuffer.destroy()
        throw new UnsupportedOperationException(s"Cannot create GMem for result type ${tagR.tag}. Output buffer has been destroyed.")
      }
    resultMem
  }

  def execute[H <: Value : Tag : FromExpr, R <: Value : FromExpr : Tag]( 
    mem: GMem[H],
    fn: GFunction[GStruct.Empty, H, R]
  ): GMem[R] =
    execute[GStruct.Empty, H, R](mem, GStruct.Empty(), fn) 

  def cleanup(): Unit = {
    pipelineCache.values.foreach(_.destroy()) 
    pipelineCache.clear()
    vkContext.destroy() 
  }

