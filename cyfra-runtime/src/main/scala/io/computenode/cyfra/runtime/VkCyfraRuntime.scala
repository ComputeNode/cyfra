package io.computenode.cyfra.runtime

import io.computenode.cyfra.compiler.Compiler
import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.{Allocation, CyfraRuntime, ExpressionProgram, GExecution, GProgram}
import io.computenode.cyfra.spirvtools.SpirvToolsRunner
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline

import java.nio.channels.FileChannel
import java.nio.file.{Paths, StandardOpenOption}
import java.security.MessageDigest
import scala.collection.mutable

class VkCyfraRuntime(spirvToolsRunner: SpirvToolsRunner = SpirvToolsRunner()) extends CyfraRuntime:
  private val context = new VulkanContext()
  import context.given

  private val gProgramCache = mutable.Map[GProgram[?, ?], SpirvProgram[?, ?]]()
  private val shaderCache = mutable.Map[(Long, Long), VkShader[?]]()
  private val compiler = new Compiler()

  private[cyfra] def getOrLoadProgram[Params, L: Layout](program: GProgram[Params, L]): VkShader[L] = synchronized:

    val spirvProgram: SpirvProgram[Params, L] = program match
      case p: ExpressionProgram[Params, L] if gProgramCache.contains(p) =>
        gProgramCache(p).asInstanceOf[SpirvProgram[Params, L]]
      case p: ExpressionProgram[Params, L] => compile(p)
      case p: SpirvProgram[Params, L]      => p
      case _                               => throw new IllegalArgumentException(s"Unsupported program type: ${program.getClass.getName}")

    gProgramCache.update(program, spirvProgram)
    shaderCache.getOrElseUpdate(spirvProgram.shaderHash, VkShader(spirvProgram)).asInstanceOf[VkShader[L]]

  private def compile[Params, L: Layout](program: ExpressionProgram[Params, L]): SpirvProgram[Params, L] =
    val ExpressionProgram(body, layout, dispatch, workgroupSize) = program
    val bindings = Layout[L].toBindings(Layout[L].layoutRef).toList
    val compiled = compiler.compile(bindings, body(Layout[L].layoutRef), workgroupSize)

//    val outputPath = Paths.get("out.spv")
//    val channel = FileChannel.open(outputPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
//    channel.write(compiled)
//    channel.close()
//    println(s"SPIR-V bytecode written to $outputPath")

    val optimizedShaderCode = spirvToolsRunner.processShaderCodeWithSpirvTools(compiled)
    SpirvProgram((il: InitProgramLayout) ?=> layout(il), dispatch, optimizedShaderCode)

  override def withAllocation(f: Allocation => Unit): Unit =
    context.withThreadContext: threadContext =>
      val executionHandler = new ExecutionHandler(this, threadContext, context)
      val allocation = new VkAllocation(threadContext.commandPool, executionHandler)
      try f(allocation)
      finally allocation.close()

  def close(): Unit =
    shaderCache.values.foreach(_.underlying.destroy())
    context.destroy()

object VkCyfraRuntime:
  def using[T](f: VkCyfraRuntime ?=> T): T =
    val runtime = new VkCyfraRuntime()
    try f(using runtime)
    finally runtime.close()
