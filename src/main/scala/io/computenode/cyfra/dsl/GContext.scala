package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Expression.Dynamic
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.vulkan.{VulkanContext, compute}
import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.*
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import izumi.reflect.Tag

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.language.postfixOps
trait Executable[H <: Value, R <: Value] {
  def execute(input: GMem[H], output: WritableGMem[R, _]): Future[Unit]
}

trait GContext {
  val vkContext = new VulkanContext(enableValidationLayers = true)
  def compile[G <: GStruct[G] : Tag: GStructSchema, H <: Value: Tag: FromExpr, R <: Value: Tag: FromExpr](function: GFunction[G, H, R]): ComputePipeline
}

val WorkerIndexTag = "worker_index"
val WorkerIndex: Int32 = Int32(Dynamic(WorkerIndexTag))
val UniformStructRefTag = "uniform_struct"
def UniformStructRef[G <: Value : Tag] = Dynamic(UniformStructRefTag)

class UniformContext[G <: GStruct[G] : Tag: GStructSchema](val uniform: G)
object UniformContext:
  def withUniform[G <: GStruct[G] : Tag: GStructSchema, T](uniform: G)(fn: UniformContext[G] ?=> T): T =
    fn(using UniformContext(uniform))
  given empty: UniformContext[Empty] = new UniformContext(Empty())

case class Empty()extends GStruct[Empty]
object Empty:
  given GStructSchema[Empty] = derived

class MVPContext extends GContext {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))

  override def compile[G <: GStruct[G] : Tag : GStructSchema, H <: Value : Tag : FromExpr, R <: Value : Tag : FromExpr](function: GFunction[G, H, R]): ComputePipeline = {
    val uniformStructSchema = summon[GStructSchema[G]]
    val uniformStruct = uniformStructSchema.fromTree(UniformStructRef)
    val tree = function.fn.apply(uniformStruct, (WorkerIndex mod function.width, WorkerIndex / function.width), new GArray2D(function.width, function.height, GArray[H](0)))
    val shaderCode = DSLCompiler.compile(tree, function.arrayInputs, function.arrayOutputs, uniformStructSchema)
    dumpSpvToFile(shaderCode, "program.spv")
    val inOut = 0 to 1 map (Binding(_, InputBufferSize(typeStride(summon[Tag[H]]))))
    val uniform = Option.when(uniformStructSchema.fields.nonEmpty)(Binding(2, UniformSize(uniformStructSchema.totalStride)))
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
