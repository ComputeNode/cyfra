package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.SpirvProgram.*
import io.computenode.cyfra.core.binding.{BufferRef, UniformRef}
import io.computenode.cyfra.core.{GExecution, GProgram}
import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.runtime.ExecutionHandler.{
  BindingLogicError,
  Dispatch,
  DispatchType,
  ExecutionBinding,
  ExecutionStep,
  PipelineBarrier,
  ShaderCall,
}
import io.computenode.cyfra.runtime.ExecutionHandler.DispatchType.*
import io.computenode.cyfra.runtime.ExecutionHandler.ExecutionBinding.{BufferBinding, UniformBinding}
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.vulkan.{VulkanContext, VulkanThreadContext}
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.core.Queue
import io.computenode.cyfra.vulkan.memory.{DescriptorPool, DescriptorPoolManager, DescriptorSet, DescriptorSetManager}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import izumi.reflect.Tag
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK13.{VK_ACCESS_2_SHADER_READ_BIT, VK_ACCESS_2_SHADER_WRITE_BIT, VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT, vkCmdPipelineBarrier2}
import org.lwjgl.vulkan.{VkCommandBuffer, VkCommandBufferBeginInfo, VkDependencyInfo, VkMemoryBarrier2, VkSubmitInfo}

import scala.collection.mutable

class ExecutionHandler(runtime: VkCyfraRuntime, threadContext: VulkanThreadContext, context: VulkanContext):
  import context.given

  private val dsManager: DescriptorSetManager = threadContext.descriptorSetManager
  private val commandPool: CommandPool = threadContext.commandPool

  def handle[Params, EL <: Layout: LayoutBinding, RL <: Layout: LayoutBinding](execution: GExecution[Params, EL, RL], params: Params, layout: EL)(
    using VkAllocation,
  ): RL =
    val (result, shaderCalls) = interpret(execution, params, layout)

    val descriptorSets = shaderCalls.map { case ShaderCall(pipeline, layout, _) =>
      pipeline.pipelineLayout.sets.map(dsManager.allocate).zip(layout).map { case (set, bindings) =>
        set.update(bindings.map(x => VkAllocation.getUnderlying(x.binding)))
        set
      }
    }

    val dispatches: Seq[Dispatch] = shaderCalls
      .zip(descriptorSets)
      .map:
        case (ShaderCall(pipeline, layout, dispatch), sets) =>
          Dispatch(pipeline, layout, sets, dispatch)

    val (executeSteps, _) = dispatches.foldLeft((Seq.empty[ExecutionStep], Set.empty[GBinding[?]])):
      case ((steps, dirty), step) =>
        val bindings = step.layout.flatten.map(_.binding)
        if bindings.exists(dirty.contains) then (steps.appendedAll(Seq(PipelineBarrier, step)), bindings.toSet)
        else (steps.appended(step), dirty ++ bindings)

    val commandBuffer = recordCommandBuffer(executeSteps)
    pushStack: stack =>
      val pCommandBuffer = stack.callocPointer(1).put(0, commandBuffer)
      val submitInfo = VkSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pCommandBuffers(pCommandBuffer)

      val fence = new Fence()
      timed("Vulkan render command"):
        check(vkQueueSubmit(commandPool.queue.get, submitInfo, fence.get), "Failed to submit command buffer to queue")
        fence.block().destroy()
    commandPool.freeCommandBuffer(commandBuffer)
    descriptorSets.flatten.foreach(dsManager.free)
    result

  private def interpret[Params, EL <: Layout: LayoutBinding, RL <: Layout: LayoutBinding](
    execution: GExecution[Params, EL, RL],
    params: Params,
    layout: EL,
  )(using VkAllocation): (RL, Seq[ShaderCall]) =
    val bindingsAcc: mutable.Map[GBinding[?], mutable.Buffer[GBinding[?]]] = mutable.Map.empty

    def mockBindings[L <: Layout: LayoutBinding](layout: L): L =
      val mapper = summon[LayoutBinding[L]]
      val res = mapper
        .toBindings(layout)
        .map:
          case x: ExecutionBinding[?] => x
          case x: GBinding[?]         =>
            val e = ExecutionBinding(x)(using x.fromExpr, x.tag)
            bindingsAcc.put(e, mutable.Buffer(x))
            e

      mapper.fromBindings(res)

    // noinspection TypeParameterShadow
    def interpretImpl[Params, EL <: Layout: LayoutBinding, RL <: Layout: LayoutBinding](
      execution: GExecution[Params, EL, RL],
      params: Params,
      layout: EL,
    ): (RL, Seq[ShaderCall]) =
      execution match
        case GExecution.Pure()                           => (layout, Seq.empty)
        case GExecution.Map(innerExec, map, cmap, cmapP) =>
          val pel = innerExec.layoutBinding
          val prl = innerExec.resLayoutBinding
          val cParams = cmapP(params)
          val cLayout = mockBindings(cmap(layout))(using pel)
          val (prevRl, calls) = interpretImpl(innerExec, cParams, cLayout)(using pel, prl)
          val nextRl = mockBindings(map(prevRl))
          (nextRl, calls)
        case GExecution.FlatMap(execution, f) =>
          val el = execution.layoutBinding
          val (rl, calls) = interpretImpl(execution, params, layout)(using el, execution.resLayoutBinding)
          val nextExecution = f(params, rl)
          val (rl2, calls2) = interpretImpl(nextExecution, params, layout)(using el, nextExecution.resLayoutBinding)
          (rl2, calls ++ calls2)
        case program: GProgram[Params, EL] =>
          given lb: LayoutBinding[EL] = program.layoutBinding
          given LayoutStruct[EL] = program.layoutStruct
          val shader =
            runtime.getOrLoadProgram(program)
          val layoutInit =
            val initProgram: InitProgramLayout = summon[VkAllocation].getInitProgramLayout
            program.layout(initProgram)(params)
          lb.toBindings(layout)
            .zip(lb.toBindings(layoutInit))
            .foreach:
              case (binding, initBinding) =>
                bindingsAcc(binding).append(initBinding)
          val dispatch = program.dispatch(layout, params) match
            case GProgram.DynamicDispatch(buffer, offset) => DispatchType.Indirect(buffer, offset)
            case GProgram.StaticDispatch(size)            => DispatchType.Direct(size._1, size._2, size._3)
          // noinspection ScalaRedundantCast
          (layout.asInstanceOf[RL], Seq(ShaderCall(shader.underlying, shader.shaderBindings(layout), dispatch)))
        case _ => ???

    val (rl, steps) = interpretImpl(execution, params, mockBindings(layout))
    val bingingToVk = bindingsAcc.map(x => (x._1, interpretBinding(x._1, x._2.toSeq)))

    val nextSteps = steps.map:
      case ShaderCall(pipeline, layout, dispatch) =>
        val nextLayout = layout.map:
          _.map:
            case Binding(binding, operation) => Binding(bingingToVk(binding), operation)
        val nextDispatch = dispatch match
          case x: Direct                => x
          case Indirect(buffer, offset) => Indirect(bingingToVk(buffer), offset)
        ShaderCall(pipeline, nextLayout, nextDispatch)

    val mapper = summon[LayoutBinding[RL]]
    val res = mapper.fromBindings(mapper.toBindings(rl).map(bingingToVk.apply))
    (res, nextSteps)

  private def interpretBinding(binding: GBinding[?], bindings: Seq[GBinding[?]])(using VkAllocation): GBinding[?] =
    binding match
      case _: BufferBinding[?] =>
        val (allocations, sizeSpec) = bindings.partitionMap:
          case x: VkBuffer[?]                  => Left(x)
          case x: GProgram.BufferLengthSpec[?] => Right(x)
          case x                               => throw BindingLogicError(x, "Unsupported buffer type")
        if allocations.size > 1 then throw BindingLogicError(allocations, "Multiple allocations for buffer")
        val alloc = allocations.headOption

        val lengths = sizeSpec.distinctBy(_.length)
        if lengths.size > 1 then throw BindingLogicError(lengths, "Multiple conflicting lengths for buffer")
        val length = lengths.headOption

        (alloc, length) match
          case (Some(buffer), Some(sizeSpec)) =>
            if buffer.length != sizeSpec.length then
              throw BindingLogicError(Seq(buffer, sizeSpec), s"Buffer length mismatch, ${buffer.length} != ${sizeSpec.length}")
            buffer
          case (Some(buffer), None) => buffer
          case (None, Some(length)) => length.materialise()
          case (None, None)         => throw new IllegalStateException("Cannot create buffer without size or allocation")

      case _: UniformBinding[?] =>
        val allocations = bindings.filter:
          case _: VkUniform[?]               => true
          case _: GProgram.DynamicUniform[?] => false
          case _: GUniform.ParamUniform[?]   => false
          case x                             => throw BindingLogicError(x, "Unsupported binding type")
        if allocations.size > 1 then throw BindingLogicError(allocations, "Multiple allocations for uniform")
        allocations.headOption.getOrElse(throw new IllegalStateException("Uniform never allocated"))
      case x => throw new IllegalArgumentException(s"Binding of type ${x.getClass.getName} should not be here")

  private def recordCommandBuffer(steps: Seq[ExecutionStep]): VkCommandBuffer = pushStack: stack =>
    val commandBuffer = commandPool.createCommandBuffer()
    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(0)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")

    steps.foreach:
      case PipelineBarrier =>
        val memoryBarrier = VkMemoryBarrier2 // TODO don't synchronise everything
          .calloc(1, stack)
          .sType$Default()
          .srcStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)
          .srcAccessMask(VK_ACCESS_2_SHADER_READ_BIT | VK_ACCESS_2_SHADER_WRITE_BIT)
          .dstStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)
          .dstAccessMask(VK_ACCESS_2_SHADER_READ_BIT | VK_ACCESS_2_SHADER_WRITE_BIT)

        val dependencyInfo = VkDependencyInfo
          .calloc(stack)
          .sType$Default()
          .pMemoryBarriers(memoryBarrier)

        vkCmdPipelineBarrier2(commandBuffer, dependencyInfo)

      case Dispatch(pipeline, layout, descriptorSets, dispatch) =>
        vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline.get)

        val pDescriptorSets = stack.longs(descriptorSets.map(_.get)*)
        vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline.pipelineLayout.id, 0, pDescriptorSets, null)

        dispatch match
          case Direct(x, y, z)          => vkCmdDispatch(commandBuffer, x, y, z)
          case Indirect(buffer, offset) => vkCmdDispatchIndirect(commandBuffer, VkAllocation.getUnderlying(buffer).get, offset)

    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")
    commandBuffer

object ExecutionHandler:
  case class ShaderCall(pipeline: ComputePipeline, layout: ShaderLayout, dispatch: DispatchType)

  sealed trait ExecutionStep
  case class Dispatch(pipeline: ComputePipeline, layout: ShaderLayout, descriptorSets: Seq[DescriptorSet], dispatch: DispatchType)
      extends ExecutionStep
  case object PipelineBarrier extends ExecutionStep

  sealed trait DispatchType
  object DispatchType:
    case class Direct(x: Int, y: Int, z: Int) extends DispatchType
    case class Indirect(buffer: GBinding[?], offset: Int) extends DispatchType

  sealed trait ExecutionBinding[T <: Value: {FromExpr, Tag}]
  object ExecutionBinding:
    class UniformBinding[T <: Value: {FromExpr, Tag}] extends ExecutionBinding[T] with GUniform[T]
    class BufferBinding[T <: Value: {FromExpr, Tag}] extends ExecutionBinding[T] with GBuffer[T]

    def apply[T <: Value: {FromExpr, Tag}](binding: GBinding[T]): ExecutionBinding[T] & GBinding[T] = binding match
      case _: GUniform[T] => new UniformBinding()
      case _: GBuffer[T]  => new BufferBinding()

  case class BindingLogicError(bindings: Seq[GBinding[?]], message: String) extends RuntimeException(s"Error in binding logic for $bindings: $message")
  object BindingLogicError:
    def apply(binding: GBinding[?], message: String): BindingLogicError =
      new BindingLogicError(Seq(binding), message)
