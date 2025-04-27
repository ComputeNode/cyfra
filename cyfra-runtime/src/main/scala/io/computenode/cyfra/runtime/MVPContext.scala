package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Expression.Dynamic
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.vulkan.{VulkanContext, compute}
import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.GStruct.Empty
import io.computenode.cyfra.runtime.mem.GMem.totalStride
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.{UniformStructRef, WorkerIndex}
import izumi.reflect.Tag

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.language.postfixOps

class MVPContext extends GContext {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))

  override def compile[G <: GStruct[G] : Tag : GStructSchema, H <: Value : Tag : FromExpr, R <: Value : Tag : FromExpr](function: GFunction[G, H, R]): ComputePipeline = {
    val uniformStructSchema = summon[GStructSchema[G]]
    val uniformStruct = uniformStructSchema.fromTree(UniformStructRef)
    val tree = function.fn.apply(uniformStruct, (WorkerIndex mod function.width, WorkerIndex / function.width), new GArray2D(function.width, function.height, GArray[H](0)))
    val shaderCode = DSLCompiler.compile(tree, function.arrayInputs, function.arrayOutputs, uniformStructSchema)
    dumpSpvToFile(shaderCode, "program.spv")
    val inOut = 0 to 1 map (Binding(_, InputBufferSize(typeStride(summon[Tag[H]]))))
    val uniform = Option.when(uniformStructSchema.fields.nonEmpty)(Binding(2, UniformSize(totalStride(uniformStructSchema))))
    val layoutInfo = LayoutInfo(Seq(LayoutSet(0, inOut ++ uniform)))
    val shader = new Shader(shaderCode, new org.joml.Vector3i(256, 1, 1), layoutInfo, "main", vkContext.device)
    new ComputePipeline(shader, vkContext)
  }

  def dumpSpvToFile(code: ByteBuffer, path: String): Unit =
    val fc: FileChannel = new FileOutputStream("program.spv").getChannel
    fc.write(code)
    fc.close()
    code.rewind()

}
