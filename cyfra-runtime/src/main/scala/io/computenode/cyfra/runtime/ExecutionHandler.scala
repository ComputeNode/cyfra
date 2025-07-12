package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.SpirvProgram.*
import io.computenode.cyfra.core.binding.{BufferRef, UniformRef}
import io.computenode.cyfra.core.{GExecution, GProgram}
import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.runtime.ExecutionHandler.{BindingLogicError, Dispatch, DispatchType, ExecutionStep, PipelineBarrier, ShaderCall}
import io.computenode.cyfra.runtime.ExecutionHandler.DispatchType.*
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.core.Queue
import io.computenode.cyfra.vulkan.memory.{DescriptorPool, DescriptorSet}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.vulkan.KHRSynchronization2.vkCmdPipelineBarrier2KHR
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK13.{VK_ACCESS_2_SHADER_READ_BIT, VK_ACCESS_2_SHADER_WRITE_BIT, VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT}
import org.lwjgl.vulkan.{VkCommandBuffer, VkCommandBufferBeginInfo, VkDependencyInfo, VkMemoryBarrier2, VkSubmitInfo}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

class ExecutionHandler(runtime: VkCyfraRuntime):
  private val context = runtime.context
  import context.given

  private val queue: Queue = context.computeQueue // TODO queue multithreading
  private val descriptorPool: DescriptorPool = context.descriptorPool // TODO descriptor pool manager
  private val commandPool: CommandPool = context.commandPool

  def handle[Params, EL <: Layout, RL <: Layout](execution: GExecution[Params, EL, RL], params: Params, layout: EL)(using VkAllocation): RL =
    pushStack: stack =>
      val (result, shaderCalls) = interpret(execution, params, layout)

      val descriptorSets = shaderCalls.map { case ShaderCall(pipeline, layout, _) =>
        pipeline.pipelineLayout.sets.map(descriptorPool.allocate).zip(layout).map { case (set, bindings) =>
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
          if bindings.exists(dirty.contains) then (steps.appendedAll(Seq(PipelineBarrier, step)), Set.empty[GBinding[?]])
          else (steps.appended(step), dirty ++ bindings)

      val commandBuffer = recordCommandBuffer(executeSteps)

      val pCommandBuffer = stack.callocPointer(1).put(0, commandBuffer)
      val submitInfo = VkSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pCommandBuffers(pCommandBuffer)

      val fence = new Fence()
      timed("Vulkan render command"):
        check(vkQueueSubmit(queue.get, submitInfo, fence.get), "Failed to submit command buffer to queue")
        fence.block()
      fence.destroy()

      commandPool.freeCommandBuffer(commandBuffer)

      result

  private def interpret[Params, EL <: Layout, RL <: Layout](execution: GExecution[Params, EL, RL], params: Params, layout: EL)(using
    VkAllocation,
  ): (RL, Seq[ShaderCall]) =
    val bindingsAcc: mutable.Map[GBinding[?], mutable.Buffer[GBinding[?]]] = mutable.Map.from(layout.bindings.map(x => (x, mutable.Buffer.empty)))

    // noinspection TypeParameterShadow
    def interpretImpl[Params, EL <: Layout, RL <: Layout](execution: GExecution[Params, EL, RL], params: Params, layout: EL): (RL, Seq[ShaderCall]) =
      execution match
        case GExecution.Pure()                           => (layout, Seq.empty)
        case GExecution.Map(execution, map, cmap, cmapP) =>
          val cParams = cmapP(params)
          val cLayout = cmap(layout)
          cLayout.bindings.foreach(x => bindingsAcc.getOrElseUpdate(x, mutable.Buffer.empty))
          val (prevLayout, calls) = interpretImpl(execution, cParams, cLayout)
          (map(prevLayout), calls)
        case GExecution.FlatMap(execution, f) =>
          val (prevLayout, calls) = interpretImpl(execution, params, layout)
          val nextExecution = f(params, prevLayout)
          val (prevLayout2, calls2) = interpretImpl(nextExecution, params, layout)
          (prevLayout2, calls ++ calls2)
        case program: GProgram[Params, EL] =>
          val shader =
            given LayoutStruct[EL] = program.layoutStruct
            runtime.getOrLoadProgram(program)
          val layoutInit =
            val initProgram: InitProgramLayout = summon[VkAllocation].getInitProgramLayout
            program.layout(initProgram)(params)
          layout.bindings
            .zip(layoutInit.bindings)
            .foreach:
              case (binding, initBinding) =>
                bindingsAcc(binding).append(initBinding)
          val dispatch = program.dispatch(layout, params) match
            case GProgram.DynamicDispatch(buffer, offset) => DispatchType.Indirect(buffer, offset)
            case GProgram.StaticDispatch(size)            => DispatchType.Direct(size._1, size._2, size._3)
          // noinspection ScalaRedundantCast
          val rl = layout.asInstanceOf[RL]
          (rl, Seq(ShaderCall(shader.underlying, shader.shaderBindings(layout), dispatch)))
        case _ => ???

    val (rl, steps) = interpretImpl(execution, params, layout)
    val bindingsMap = bindingsAcc.view.mapValues(_.toSeq).map(x => (x._1, interpretBinding(x._1, x._2))).toMap

    val nextSteps = steps.map:
      case ShaderCall(pipeline, layout, dispatch) =>
        val nextLayout = layout.map:
          _.map:
            case Binding(binding, operation) => Binding(bindingsMap(binding), operation)
        val nextDispatch = dispatch match
          case x: Direct                => x
          case Indirect(buffer, offset) => Indirect(bindingsMap(buffer), offset)
        ShaderCall(pipeline, nextLayout, nextDispatch)

    (rl, nextSteps)

  private def interpretBinding(binding: GBinding[?], limiters: Seq[GBinding[?]])(using VkAllocation): GBinding[?] =
    val bindings = limiters.appended(binding)
    binding match
      case buffer: GBuffer[?] =>
        val (allocations, sizeSpec) = bindings.partitionMap:
          case x: VkBuffer[?]                  => Left(x)
          case x: GProgram.BufferLengthSpec[?] => Right(x)
          case x                               => throw BindingLogicError(x, "Unsupported buffer type")
        if allocations.size > 1 then throw BindingLogicError(allocations, "Multiple allocations for buffer")
        val all = allocations.headOption

        val lengths = sizeSpec.distinctBy(_.length)
        if lengths.size > 1 then throw BindingLogicError(lengths, "Multiple conflicting lengths for buffer")
        val length = lengths.headOption

        (all, length) match
          case (Some(buffer), Some(sizeSpec)) =>
            if buffer.length != sizeSpec.length then
              throw BindingLogicError(Seq(buffer, sizeSpec), s"Buffer length mismatch, ${buffer.length} != ${sizeSpec.length}")
            buffer
          case (Some(buffer), None) => buffer
          case (None, Some(length)) => ???
          case (None, None)         => throw BindingLogicError(binding, "Cannot create buffer without size or allocation")

      case uniform: GUniform[?] =>
        val allocations = bindings.filter:
          case _: VkUniform[?]               => true
          case _: GProgram.DynamicUniform[?] => false
          case _: GUniform.ParamUniform[?]   => false
          case x                             => throw BindingLogicError(x, "Unsupported binding type")
        if allocations.size > 1 then throw BindingLogicError(allocations, "Multiple allocations for uniform")
        allocations.headOption.getOrElse(throw BindingLogicError(binding, "Uniform never allocated"))

  private def recordCommandBuffer(steps: Seq[ExecutionStep]): VkCommandBuffer = pushStack: stack =>
    val commandBuffer = commandPool.createCommandBuffer()
    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(0)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")

    steps.foreach:
      case PipelineBarrier => // TODO WaR and WaW errors
        val memoryBarrier = VkMemoryBarrier2
          .calloc(1, stack)
          .sType$Default()
          .srcStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)
          .srcAccessMask(VK_ACCESS_2_SHADER_WRITE_BIT)
          .dstStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)
          .dstAccessMask(VK_ACCESS_2_SHADER_READ_BIT)

        val dependencyInfo = VkDependencyInfo
          .calloc(stack)
          .sType$Default()
          .pMemoryBarriers(memoryBarrier)

        vkCmdPipelineBarrier2KHR(commandBuffer, dependencyInfo)

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

  case class BindingLogicError(bindings: Seq[GBinding[?]], message: String) extends RuntimeException(s"Error in binding logic for $bindings: $message")
  object BindingLogicError:
    def apply(binding: GBinding[?], message: String): BindingLogicError =
      new BindingLogicError(Seq(binding), message)
