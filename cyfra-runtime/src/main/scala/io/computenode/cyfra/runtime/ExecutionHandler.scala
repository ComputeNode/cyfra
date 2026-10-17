package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.{GExecution, GProgram, Layout}
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.memory.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.runtime.ExecutionHandler.{
  BindingLogicError,
  BufferCopyCall,
  BufferCopyStep,
  Dispatch,
  DispatchType,
  ExecutionBinding,
  ExecutionCall,
  ExecutionStep,
  PipelineBarrier,
  ShaderCall,
}
import io.computenode.cyfra.runtime.ExecutionHandler.DispatchType.*
import io.computenode.cyfra.runtime.ExecutionHandler.ExecutionBinding.{BufferBinding, UniformBinding}
import io.computenode.cyfra.runtime.SpirvProgram.*
import io.computenode.cyfra.vulkan.{VulkanContext, VulkanThreadContext}
import io.computenode.cyfra.vulkan.command.{CommandPool, Semaphore}
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.memory.{DescriptorSet, DescriptorSetManager}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK13.{
  VK_ACCESS_2_SHADER_READ_BIT,
  VK_ACCESS_2_SHADER_WRITE_BIT,
  VK_ACCESS_2_TRANSFER_READ_BIT,
  VK_ACCESS_2_TRANSFER_WRITE_BIT,
  VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT,
  VK_PIPELINE_STAGE_2_TRANSFER_BIT,
  vkCmdPipelineBarrier2,
}
import org.lwjgl.vulkan.EXTDebugUtils.{vkCmdBeginDebugUtilsLabelEXT, vkCmdEndDebugUtilsLabelEXT}
import org.lwjgl.vulkan.{
  VkBufferCopy,
  VkCommandBuffer,
  VkCommandBufferBeginInfo,
  VkDebugUtilsLabelEXT,
  VkDependencyInfo,
  VkMemoryBarrier2,
  VkSubmitInfo,
  VkTimelineSemaphoreSubmitInfo,
}

import scala.collection.mutable

class ExecutionHandler(runtime: VkCyfraRuntime, threadContext: VulkanThreadContext, context: VulkanContext):
  import context.given

  private val dsManager: DescriptorSetManager = threadContext.descriptorSetManager
  private val commandPool: CommandPool.Reset = threadContext.commandPool
  private val queue = commandPool.queue

  // Timeline semaphore for GPU-GPU synchronization (no CPU blocking between submissions)
  private val timelineSemaphore = new Semaphore()
  private var semaphoreValue: Long = 0

  private case class CachedExecution(resultBindings: Seq[GBinding[?]], commandBuffer: VkCommandBuffer, executeSteps: Seq[ExecutionStep])
  private val executionCache = new java.util.IdentityHashMap[GExecution[?, ?, ?], mutable.Map[(Any, Seq[GBinding[?]]), CachedExecution]]()

  def handle[Params, EL: Layout, RL: Layout](execution: GExecution[Params, EL, RL], params: Params, layout: EL)(using VkAllocation): RL =
    val layoutBindings = Layout[EL].toBindings(layout)
    val cache = executionCache.computeIfAbsent(execution, _ => mutable.Map.empty)
    val cacheKey = (params, layoutBindings)

    cache.get(cacheKey) match
      case Some(cached) =>
        summon[VkAllocation].submitPendingWrites()
        submit(cached.commandBuffer)
        Layout[RL].fromBindings(cached.resultBindings)

      case None =>
        // Cache miss - full execution path
        val (result, executionCalls) = interpret(execution, params, layout)

        // Convert ExecutionCalls to ExecutionSteps
        val initialSteps: Seq[ExecutionStep] = executionCalls.map:
          case ShaderCall(pipeline, layout, dispatch) =>
            val sets = pipeline.pipelineLayout.sets
              .map(dsManager.allocate)
              .zip(layout)
              .map:
                case (set, bindings) =>
                  set.update(bindings.map(x => VkAllocation.getUnderlying(x.binding).buffer))
                  set
            Dispatch(pipeline, layout, sets, dispatch)
          case BufferCopyCall(src, dst, sizeBytes) =>
            BufferCopyStep(src, dst, sizeBytes)

        val executeSteps = ExecutionHandler.insertBarriers(initialSteps)

        val commandBuffer = recordCommandBuffer(executeSteps)

        summon[VkAllocation].submitPendingWrites()
        submit(commandBuffer)

        // Cache the execution
        val resultBindings = Layout[RL].toBindings(result)
        cache(cacheKey) = CachedExecution(resultBindings, commandBuffer, executeSteps)

        result

  private def submit(commandBuffer: VkCommandBuffer): Unit =
    val previous = semaphoreValue
    semaphoreValue += 1
    submitWithSemaphore(commandBuffer, previous, semaphoreValue)

  /** Submit command buffer with timeline semaphore wait/signal for GPU-GPU pipelining */
  private def submitWithSemaphore(commandBuffer: VkCommandBuffer, waitValue: Long, signalValue: Long): Unit = pushStack: stack =>
    val timelineInfo = VkTimelineSemaphoreSubmitInfo
      .calloc(stack)
      .sType$Default()
      .waitSemaphoreValueCount(1)
      .pWaitSemaphoreValues(stack.longs(waitValue))
      .signalSemaphoreValueCount(1)
      .pSignalSemaphoreValues(stack.longs(signalValue))

    val submitInfo = VkSubmitInfo
      .calloc(1, stack)
      .sType$Default()
      .pNext(timelineInfo)
      .waitSemaphoreCount(1)
      .pWaitSemaphores(stack.longs(timelineSemaphore.get))
      .pWaitDstStageMask(stack.ints(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT))
      .pCommandBuffers(stack.pointers(commandBuffer))
      .pSignalSemaphores(stack.longs(timelineSemaphore.get))

    check(vkQueueSubmit(queue.get, submitInfo, VK_NULL_HANDLE), "Failed to submit command buffer")

  /** Wait for all GPU work to complete (call before reading results) */
  def sync(): Unit =
    if semaphoreValue > 0 then timelineSemaphore.waitValue(semaphoreValue)

  def close(): Unit =
    sync()
    val caches = executionCache.values().iterator()
    while caches.hasNext do
      caches
        .next()
        .values
        .foreach: cached =>
          cached.executeSteps.foreach:
            case Dispatch(_, _, sets, _) => sets.foreach(dsManager.free)
            case _                       => ()
          commandPool.freeCommandBuffer(cached.commandBuffer)
    executionCache.clear()
    timelineSemaphore.close()

  private def interpret[Params, EL: Layout, RL: Layout](execution: GExecution[Params, EL, RL], params: Params, layout: EL)(using
    VkAllocation,
  ): (RL, Seq[ExecutionCall]) =
    val bindingsAcc: mutable.Map[GBinding[?], mutable.Buffer[GBinding[?]]] = mutable.Map.empty

    def mockBindings[L: Layout](layout: L): L =
      val mapper = Layout[L]
      val res = mapper
        .toBindings(layout)
        .map:
          case x: ExecutionBinding[?] => x
          case x: GBinding[?]         =>
            val e = ExecutionBinding(x)(using x.v)
            bindingsAcc.put(e, mutable.Buffer(x))
            e
      mapper.fromBindings(res)

    // noinspection TypeParameterShadow
    def interpretImpl[Params, EL: Layout, RL: Layout](execution: GExecution[Params, EL, RL], params: Params, layout: EL): (RL, Seq[ExecutionCall]) =
      execution match
        case GExecution.Pure()                           => (layout, Seq.empty)
        case GExecution.Map(innerExec, map, cmap, cmapP) =>
          val pel = innerExec.execLayout
          val prl = innerExec.resLayout
          val cParams = cmapP(params)
          val cLayout = mockBindings(cmap(layout))(using pel)
          val (prevRl, calls) = interpretImpl(innerExec, cParams, cLayout)(using pel, prl)
          val nextRl = mockBindings(map(prevRl))
          (nextRl, calls)
        case GExecution.FlatMap(execution, f) =>
          val el = execution.execLayout
          val (rl, calls) = interpretImpl(execution, params, layout)(using el, execution.resLayout)
          val nextExecution = f(params, rl)
          val (rl2, calls2) = interpretImpl(nextExecution, params, layout)(using el, nextExecution.resLayout)
          (rl2, calls ++ calls2)
        case program: GProgram[Params, EL] =>
          given lb: Layout[EL] = program.execLayout
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
        case bufferCopy: GExecution.BufferCopy[EL] =>
          val (src, dst) = bufferCopy.getBuffers(layout)
          // noinspection ScalaRedundantCast
          (layout.asInstanceOf[RL], Seq(BufferCopyCall(src, dst, bufferCopy.sizeBytes)))
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
      case BufferCopyCall(src, dst, sizeBytes) =>
        BufferCopyCall(bingingToVk(src), bingingToVk(dst), sizeBytes)

    val mapper = Layout[RL]
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
        allocations.headOption.getOrElse(throw new BindingLogicError(Seq(), "Uniform never allocated"))
      case x => throw new IllegalArgumentException(s"Binding of type ${x.getClass.getName} should not be here")

  private def recordCommandBuffer(steps: Seq[ExecutionStep]): VkCommandBuffer = pushStack: stack =>
    val commandBuffer = commandPool.createCommandBuffer()
    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")
    def recordStep(step: ExecutionStep): Unit = pushStack: stack =>
      step match
        case PipelineBarrier =>
          val memoryBarrier = VkMemoryBarrier2 // TODO don't synchronise everything
            .calloc(1, stack)
            .sType$Default()
            .srcStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT | VK_PIPELINE_STAGE_2_TRANSFER_BIT)
            .srcAccessMask(VK_ACCESS_2_SHADER_READ_BIT | VK_ACCESS_2_SHADER_WRITE_BIT | VK_ACCESS_2_TRANSFER_READ_BIT | VK_ACCESS_2_TRANSFER_WRITE_BIT)
            .dstStageMask(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT | VK_PIPELINE_STAGE_2_TRANSFER_BIT)
            .dstAccessMask(VK_ACCESS_2_SHADER_READ_BIT | VK_ACCESS_2_SHADER_WRITE_BIT | VK_ACCESS_2_TRANSFER_READ_BIT | VK_ACCESS_2_TRANSFER_WRITE_BIT)

          val debugLabel = VkDebugUtilsLabelEXT
            .calloc(stack)
            .sType$Default()
            .pLabelName(stack.UTF8("BARRIER"))
          vkCmdBeginDebugUtilsLabelEXT(commandBuffer, debugLabel)

          val dependencyInfo = VkDependencyInfo
            .calloc(stack)
            .sType$Default()
            .pMemoryBarriers(memoryBarrier)

          vkCmdPipelineBarrier2(commandBuffer, dependencyInfo)

          vkCmdEndDebugUtilsLabelEXT(commandBuffer)

        case Dispatch(pipeline, layout, descriptorSets, dispatch) =>
          // Add debug label for profiling (visible in Nsight Systems)
          val dispatchSize = dispatch match
            case Direct(x, y, z) => s"${x}x${y}x$z"
            case Indirect(_, _)  => "indirect"
          val labelName = s"${pipeline.name}[$dispatchSize]"
          val debugLabel = VkDebugUtilsLabelEXT
            .calloc(stack)
            .sType$Default()
            .pLabelName(stack.UTF8(labelName))
          vkCmdBeginDebugUtilsLabelEXT(commandBuffer, debugLabel)

          vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline.get)

          val pDescriptorSets = stack.longs(descriptorSets.map(_.get)*)
          vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline.pipelineLayout.id, 0, pDescriptorSets, null)

          dispatch match
            case Direct(x, y, z)          => vkCmdDispatch(commandBuffer, x, y, z)
            case Indirect(buffer, offset) => vkCmdDispatchIndirect(commandBuffer, VkAllocation.getUnderlying(buffer).buffer.get, offset)

          vkCmdEndDebugUtilsLabelEXT(commandBuffer)

        case BufferCopyStep(src, dst, sizeBytes) =>
          // Add debug label for profiling
          val debugLabel = VkDebugUtilsLabelEXT
            .calloc(stack)
            .sType$Default()
            .pLabelName(stack.UTF8(s"BufferCopy[${sizeBytes}B]"))
          vkCmdBeginDebugUtilsLabelEXT(commandBuffer, debugLabel)

          val copyRegion = VkBufferCopy
            .calloc(1, stack)
            .srcOffset(0)
            .dstOffset(0)
            .size(sizeBytes)

          val srcBuffer = VkAllocation.getUnderlying(src).buffer.get
          val dstBuffer = VkAllocation.getUnderlying(dst).buffer.get
          vkCmdCopyBuffer(commandBuffer, srcBuffer, dstBuffer, copyRegion)

          vkCmdEndDebugUtilsLabelEXT(commandBuffer)

    steps.foreach(recordStep)
    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")
    commandBuffer

object ExecutionHandler:
  private[runtime] def insertBarriers(steps: Seq[ExecutionStep]): Seq[ExecutionStep] =
    var accessed = Set.empty[GBinding[?]]
    var written = Set.empty[GBinding[?]]
    val result = Vector.newBuilder[ExecutionStep]
    steps.foreach: step =>
      val (reads, writes) = step match
        case Dispatch(_, layout, _, dispatch) =>
          val bindings = layout.flatten
          val indirect = dispatch match
            case Indirect(buffer, _) => Seq(buffer)
            case _                   => Seq.empty
          (bindings.filter(_.operation != Operation.Write).map(_.binding) ++ indirect, bindings.filter(_.operation != Operation.Read).map(_.binding))
        case BufferCopyStep(src, dst, _) => (Seq(src), Seq(dst))
        case PipelineBarrier             => (Seq.empty, Seq.empty)
      if reads.exists(written.contains) || writes.exists(accessed.contains) then
        result += PipelineBarrier
        accessed = Set.empty
        written = Set.empty
      result += step
      accessed ++= reads ++ writes
      written ++= writes
    result.result()

  /** Represents a call to be executed on GPU - either a shader dispatch or buffer copy. */
  sealed trait ExecutionCall
  case class ShaderCall(pipeline: ComputePipeline, layout: ShaderLayout, dispatch: DispatchType) extends ExecutionCall
  case class BufferCopyCall(src: GBinding[?], dst: GBinding[?], sizeBytes: Int) extends ExecutionCall

  sealed trait ExecutionStep
  case class Dispatch(pipeline: ComputePipeline, layout: ShaderLayout, descriptorSets: Seq[DescriptorSet], dispatch: DispatchType)
      extends ExecutionStep
  case class BufferCopyStep(src: GBinding[?], dst: GBinding[?], sizeBytes: Int) extends ExecutionStep
  case object PipelineBarrier extends ExecutionStep

  sealed trait DispatchType
  object DispatchType:
    case class Direct(x: Int, y: Int, z: Int) extends DispatchType
    case class Indirect(buffer: GBinding[?], offset: Int) extends DispatchType

  sealed trait ExecutionBinding[T: Value]
  object ExecutionBinding:
    class UniformBinding[T: Value] extends ExecutionBinding[T] with GUniform[T]
    class BufferBinding[T: Value] extends ExecutionBinding[T] with GBuffer[T]

    def apply[T: Value as v](binding: GBinding[T]): ExecutionBinding[T] & GBinding[T] = binding match
      case _: GUniform[T] => new UniformBinding()
      case _: GBuffer[T]  => new BufferBinding()

  case class BindingLogicError(bindings: Seq[GBinding[?]], message: String) extends RuntimeException(s"Error in binding logic for $bindings: $message")
  object BindingLogicError:
    def apply(binding: GBinding[?], message: String): BindingLogicError =
      new BindingLogicError(Seq(binding), message)
