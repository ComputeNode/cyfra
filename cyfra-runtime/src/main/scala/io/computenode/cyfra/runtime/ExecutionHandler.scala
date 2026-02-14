package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.SpirvProgram.*
import io.computenode.cyfra.core.binding.{BufferRef, UniformRef}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.{GExecution, GProgram, Provenance}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.runtime.ExecutionHandler.*
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.utility.Logger
import io.computenode.cyfra.vulkan.{VulkanContext, VulkanThreadContext}
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Semaphore}
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import io.computenode.cyfra.vulkan.core.Queue
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer, DescriptorPool, DescriptorPoolManager, DescriptorSet, DescriptorSetManager}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import izumi.reflect.Tag
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK13.{VK_ACCESS_2_SHADER_READ_BIT, VK_ACCESS_2_SHADER_WRITE_BIT, VK_ACCESS_2_TRANSFER_READ_BIT, VK_ACCESS_2_TRANSFER_WRITE_BIT, VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT, VK_PIPELINE_STAGE_2_TRANSFER_BIT, vkCmdPipelineBarrier2}
import org.lwjgl.vulkan.EXTDebugUtils.{vkCmdBeginDebugUtilsLabelEXT, vkCmdEndDebugUtilsLabelEXT}
import org.lwjgl.vulkan.{VkBufferCopy, VkCommandBuffer, VkCommandBufferBeginInfo, VkDebugUtilsLabelEXT, VkDependencyInfo, VkMemoryBarrier2, VkSubmitInfo, VkTimelineSemaphoreSubmitInfo}

import scala.collection.mutable

/** Handles execution of GPU programs with smart barrier insertion.
  * Builds a single command buffer from a DAG of programs and inserts barriers only when needed.
  */
class ExecutionHandler(runtime: VkCyfraRuntime, threadContext: VulkanThreadContext, context: VulkanContext):
  import context.given

  private val dsManager: DescriptorSetManager = threadContext.descriptorSetManager
  private val commandPool: CommandPool.Reset = threadContext.commandPool
  private val queue = commandPool.queue
  
  // Timeline semaphore for GPU-GPU synchronization
  private val timelineSemaphore = new Semaphore()
  private var semaphoreValue: Long = 0
  
  // Cache for prepared executions - key is the set of underlying buffer handles involved
  private val executionCache = mutable.Map[ExecutionCacheKey, CachedExecution]()
  // Cache for recorded command buffers
  private val commandBufferCache = mutable.Map[ExecutionCacheKey, VkCommandBuffer]()
  // Fast cache for command buffers - key is hash of binding identity hashcodes
  private val fastCommandBufferCache = mutable.Map[Int, VkCommandBuffer]()
  
  // === Main execution entry point ===

  /** Execute a plan for the given target bindings.
    * Builds a single command buffer with smart barrier insertion based on data hazards.
    */
  /** Fast path: check cache and submit if found, without building execution plan.
    * Returns true if cache hit, false if cache miss.
    */
  def submitIfCached(targetBindings: Seq[GBinding[?]])(using VkAllocation): Boolean =
    val fastKey = targetBindings.map(b => VkAllocation.getUnderlying(b).buffer.handle).hashCode()
    fastCommandBufferCache.get(fastKey) match
      case Some(commandBuffer) =>
        val waitValue = semaphoreValue
        semaphoreValue += 1
        submitWithSemaphore(commandBuffer, waitValue, semaphoreValue)
        true
      case None =>
        println(s"[Cache miss] fastKey=$fastKey, cacheSize=${fastCommandBufferCache.size}, handles=${targetBindings.take(5).map(b => VkAllocation.getUnderlying(b).buffer.handle).mkString(",")}")
        false

  def executePlan(targetBindings: Seq[GBinding[?]])(using VkAllocation): Unit =
    // Fast cache key: hash of UNDERLYING buffer handles (not binding identity!)
    val fastKey = targetBindings.map(b => VkAllocation.getUnderlying(b).buffer.handle).hashCode()
    
    // Check if we have a fully cached command buffer
    fastCommandBufferCache.get(fastKey) match
      case Some(commandBuffer) =>
        // Fast path - reuse cached command buffer directly
        val waitValue = semaphoreValue
        semaphoreValue += 1
        submitWithSemaphore(commandBuffer, waitValue, semaphoreValue)
        return
      case None =>
        () // Cache miss
    
    // Slow path: build everything
    val plan = buildExecutionPlan(targetBindings)
    if plan.isEmpty then return

    // Create cache key from the plan structure
    val cacheKey = createCacheKey(plan)
    
    // Get or create cached execution
    val cached = executionCache.getOrElseUpdate(cacheKey, {
      // Prepare all dispatches (compile shaders, allocate descriptor sets)
      val preparedSteps = prepareSteps(plan)
      // Analyze data hazards and group into waves
      val waves = buildWaves(preparedSteps)
      CachedExecution(preparedSteps, waves)
    })
            
    // Get or create cached command buffer
    val commandBuffer = commandBufferCache.getOrElseUpdate(cacheKey, {
      recordWavesCommandBuffer(cached.waves)
    })
    
    // Cache by fast key for next time
    fastCommandBufferCache(fastKey) = commandBuffer

    // Submit once
    val waitValue = semaphoreValue
    semaphoreValue += 1
    val signalValue = semaphoreValue
    submitWithSemaphore(commandBuffer, waitValue, signalValue)
  
  /** Create a cache key from the execution plan.
    * Two plans are equivalent if they have the same programs operating on the same underlying buffers.
    */
  private def createCacheKey(plan: Seq[PlanStep])(using VkAllocation): ExecutionCacheKey =
    val stepKeys = plan.map:
      case PlanStep.Execute(execution, _, inputs) =>
        val programKey = execution match
          case p: GProgram[?, ?] => p.name
          case _ => execution.hashCode().toString
        val bufferKeys = inputs.map(b => VkAllocation.getUnderlying(b).buffer.handle)
        StepKey.ExecuteKey(programKey, bufferKeys)
      case PlanStep.Copy(source, dest, readIdx, writeIdx, size) =>
        val srcHandle = VkAllocation.getUnderlying(source).buffer.handle
        val dstHandle = VkAllocation.getUnderlying(dest).buffer.handle
        StepKey.CopyKey(srcHandle, dstHandle, readIdx, writeIdx, size)
    ExecutionCacheKey(stepKeys)
        
  // === Provenance DAG traversal ===

  /** Build execution plan from provenance DAG.
    * Traverses provenance to find all ExecutionNodes and orders them topologically.
    */
  private def buildExecutionPlan(targetBindings: Seq[GBinding[?]]): Seq[PlanStep] =
    val visitedBindings = mutable.Set[GBinding[?]]()
    val visitedPrograms = mutable.Set[GProgram[?, ?]]()
    val plan = mutable.Buffer[PlanStep]()

    def visit(binding: GBinding[?]): Unit =
      if visitedBindings.contains(binding) then return
      visitedBindings += binding

      binding.provenance match
        case Provenance.External(_) =>
          // External bindings are already available
          ()

        case Provenance.ExecutionNode(program, params, inputs, _) =>
          // First visit all inputs (ensures topological order)
          inputs.foreach(visit)

          // Then add this program if not already added
          if !visitedPrograms.contains(program) then
            visitedPrograms += program
            plan += PlanStep.Execute(program, params, inputs)

        case Provenance.Copied(source, readIdx, writeIdx, size) =>
          visit(source)
          plan += PlanStep.Copy(source, binding, readIdx, writeIdx, size)

        case Provenance.Materialized(_, _) =>
          // Already materialized
          ()

        case _ =>
          ()

    targetBindings.foreach(visit)
    plan.toSeq

  // === Step preparation ===

  /** Prepare execution steps - compile shaders, allocate descriptor sets. */
  private def prepareSteps(plan: Seq[PlanStep])(using VkAllocation): Seq[PreparedStep] =
    plan.flatMap:
      case PlanStep.Execute(program, params, inputs) =>
        Some(prepareProgram(program, params, inputs))

      case PlanStep.Copy(source, dest, readIdx, writeIdx, size) =>
        Some(PreparedStep.CopyStep(source, dest, readIdx, writeIdx, size))

  /** Prepare a program for execution - compile shader, allocate descriptor sets. */
  private def prepareProgram[Params, L](
    program: GProgram[Params, L],
    params: Any,
    inputs: Seq[GBinding[?]],
  )(using VkAllocation): PreparedStep.DispatchStep =
    given Layout[L] = program.summonLayout
    val pipeline = runtime.getOrLoadProgram(program)
    val layoutInstance = Layout[L]

    // Reconstruct the layout from inputs
    val layout = layoutInstance.fromBindings(inputs)

    // Get shader bindings with read/write operations
    val shaderBindings = pipeline.shaderBindings(layout)

    // Build access map: binding -> operation
    val accessMap: Map[GBinding[?], Operation] = shaderBindings.flatten.map(b => b.binding -> b.operation).toMap

    // Allocate descriptor sets
    val descriptorSets = pipeline.underlying.pipelineLayout.sets
      .map(dsManager.allocate)
      .zip(shaderBindings)
      .map { case (set, bindings) =>
        set.update(bindings.map(b => VkAllocation.getUnderlying(b.binding).buffer))
        set
      }

    // Determine dispatch type - use params from provenance, not program.params!
    val dispatch = program.dispatchSize(layout, params.asInstanceOf[Params]) match
      case GProgram.DynamicDispatch(buffer, offset) => DispatchType.Indirect(buffer, offset)
      case GProgram.StaticDispatch(size)            => DispatchType.Direct(size._1, size._2, size._3)

    PreparedStep.DispatchStep(
      program = program,
      pipeline = pipeline.underlying,
      shaderBindings = shaderBindings,
      descriptorSets = descriptorSets,
      dispatchType = dispatch,
      accessMap = accessMap,
      inputs = inputs,
    )

  // === Data hazard analysis ===

  /** Build waves of dispatches that can execute without barriers between them. */
  private def buildWaves(steps: Seq[PreparedStep])(using VkAllocation): Seq[ExecutionWave] =
    if steps.isEmpty then return Seq.empty

    // Helper to get underlying Vulkan buffer - this is what we track for dependencies
    // (different GBinding instances can share the same underlying buffer)
    def getBufferKey(binding: GBinding[?]): Long =
      VkAllocation.getUnderlying(binding).buffer.handle

    // Track last writer and readers for each UNDERLYING buffer (by handle)
    val lastWriter = mutable.Map[Long, Int]()      // buffer handle -> step index that wrote
    val lastReaders = mutable.Map[Long, Set[Int]]() // buffer handle -> step indices that read

    // Build dependency graph: step index -> set of step indices it must wait for
    val dependencies = mutable.Map[Int, Set[Int]]().withDefaultValue(Set.empty)

    for (step, stepIdx) <- steps.zipWithIndex do
      val accesses: Map[GBinding[?], Operation] = step match
        case d: PreparedStep.DispatchStep => d.accessMap
        case c: PreparedStep.CopyStep     =>
          // Copy reads from source, writes to dest
          Map(c.source -> Operation.Read, c.dest -> Operation.Write)

      for (binding, op) <- accesses do
        val bufferKey = getBufferKey(binding)
        op match
          case Operation.Read =>
            // RAW: if someone wrote, we depend on them
            lastWriter.get(bufferKey).foreach: writerIdx =>
              dependencies(stepIdx) = dependencies(stepIdx) + writerIdx

          case Operation.Write =>
            // RAW: if someone wrote, we depend on them (WAW)
            lastWriter.get(bufferKey).foreach: writerIdx =>
              dependencies(stepIdx) = dependencies(stepIdx) + writerIdx
            // WAR: if someone read, we depend on them
            lastReaders.get(bufferKey).foreach: readerIdxs =>
              dependencies(stepIdx) = dependencies(stepIdx) ++ readerIdxs

          case Operation.ReadWrite =>
            // All hazards possible
            lastWriter.get(bufferKey).foreach: writerIdx =>
              dependencies(stepIdx) = dependencies(stepIdx) + writerIdx
            lastReaders.get(bufferKey).foreach: readerIdxs =>
              dependencies(stepIdx) = dependencies(stepIdx) ++ readerIdxs

        // Update tracking
        op match
          case Operation.Write | Operation.ReadWrite =>
            lastWriter(bufferKey) = stepIdx
            lastReaders(bufferKey) = Set.empty
          case Operation.Read =>
            lastReaders(bufferKey) = lastReaders.getOrElse(bufferKey, Set.empty) + stepIdx

    // Build waves using greedy algorithm
    val waves = mutable.Buffer[ExecutionWave]()
    val completed = mutable.Set[Int]()
    val remaining = mutable.Set.from(steps.indices)

    while remaining.nonEmpty do
      // Find all steps whose dependencies are satisfied
      val ready = remaining.filter: idx =>
        dependencies(idx).forall(completed.contains)

      if ready.isEmpty then
        throw new IllegalStateException("Cyclic dependency detected in execution plan")

      // Group ready steps into a wave
      waves += ExecutionWave(ready.toSeq.sorted.map(steps(_)))
      completed ++= ready
      remaining --= ready

    waves.toSeq

  // === Command buffer recording ===

  /** Record all waves into a single command buffer with barriers only between waves. */
  private def recordWavesCommandBuffer(waves: Seq[ExecutionWave])(using VkAllocation): VkCommandBuffer = pushStack: stack =>
    val commandBuffer = commandPool.createCommandBuffer()
    val commandBufferBeginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(0)

    check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")

    for (wave, waveIdx) <- waves.zipWithIndex do
      // Insert barrier BETWEEN waves (not before first)
      if waveIdx > 0 then
        recordPipelineBarrier(commandBuffer)

      // Record all steps in this wave
      for step <- wave.steps do
        step match
          case d: PreparedStep.DispatchStep =>
            recordDispatch(commandBuffer, d)
          case c: PreparedStep.CopyStep =>
            recordBufferCopy(commandBuffer, c)

    check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")
    commandBuffer

  /** Record a pipeline barrier. */
  private def recordPipelineBarrier(commandBuffer: VkCommandBuffer): Unit = pushStack: stack =>
    val memoryBarrier = VkMemoryBarrier2
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

  /** Record a dispatch. */
  private def recordDispatch(commandBuffer: VkCommandBuffer, dispatch: PreparedStep.DispatchStep)(using VkAllocation): Unit = pushStack: stack =>
    val dispatchSizeStr = dispatch.dispatchType match
      case DispatchType.Direct(x, y, z) => s"${x}x${y}x${z}"
      case DispatchType.Indirect(_, _)  => "indirect"
    val labelName = s"${dispatch.pipeline.name}[$dispatchSizeStr]"

    // Start debug label
    val debugLabel = VkDebugUtilsLabelEXT
      .calloc(stack)
      .sType$Default()
      .pLabelName(stack.UTF8(labelName))
    vkCmdBeginDebugUtilsLabelEXT(commandBuffer, debugLabel)

    // Bind pipeline and descriptor sets
    vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, dispatch.pipeline.get)
    val pDescriptorSets = stack.longs(dispatch.descriptorSets.map(_.get)*)
    vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, dispatch.pipeline.pipelineLayout.id, 0, pDescriptorSets, null)

    // Dispatch
    dispatch.dispatchType match
      case DispatchType.Direct(x, y, z)          => vkCmdDispatch(commandBuffer, x, y, z)
      case DispatchType.Indirect(buffer, offset) => vkCmdDispatchIndirect(commandBuffer, VkAllocation.getUnderlying(buffer).buffer.get, offset)

    // End debug label
    vkCmdEndDebugUtilsLabelEXT(commandBuffer)

  /** Record a buffer copy. */
  private def recordBufferCopy(commandBuffer: VkCommandBuffer, copy: PreparedStep.CopyStep)(using VkAllocation): Unit = pushStack: stack =>
    val srcBinding = VkAllocation.getUnderlying(copy.source)
    val dstBinding = VkAllocation.getUnderlying(copy.dest)
    val sizeBytes = copy.size * srcBinding.sizeOfT

    // Start debug label
    val debugLabel = VkDebugUtilsLabelEXT
      .calloc(stack)
      .sType$Default()
      .pLabelName(stack.UTF8(s"BufferCopy[${sizeBytes}B]"))
    vkCmdBeginDebugUtilsLabelEXT(commandBuffer, debugLabel)

    // Copy
    val copyRegion = VkBufferCopy
      .calloc(1, stack)
      .srcOffset(copy.readIdx * srcBinding.sizeOfT)
      .dstOffset(copy.writeIdx * dstBinding.sizeOfT)
      .size(sizeBytes)
    vkCmdCopyBuffer(commandBuffer, srcBinding.buffer.get, dstBinding.buffer.get, copyRegion)

    // End debug label
    vkCmdEndDebugUtilsLabelEXT(commandBuffer)

  // === Submission ===

  /** Submit command buffer with timeline semaphore for GPU pipelining. */
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
      .pWaitDstStageMask(stack.ints(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT))
      .pCommandBuffers(stack.pointers(commandBuffer))
      .pSignalSemaphores(stack.longs(timelineSemaphore.get))

    check(vkQueueSubmit(queue.get, submitInfo, VK_NULL_HANDLE), "Failed to submit command buffer")

  /** Wait for all GPU work to complete. */
  def sync(): Unit =
    if semaphoreValue > 0 then timelineSemaphore.waitValue(semaphoreValue)

object ExecutionHandler:

  // === Plan steps (before preparation) ===

  sealed trait PlanStep
  object PlanStep:
    case class Execute(program: GProgram[?, ?], params: Any, inputs: Seq[GBinding[?]]) extends PlanStep
    case class Copy(source: GBinding[?], dest: GBinding[?], readIdx: Int, writeIdx: Int, size: Int) extends PlanStep

  // === Prepared steps (ready for recording) ===

  sealed trait PreparedStep
  object PreparedStep:
    case class DispatchStep(
      program: GProgram[?, ?],
      pipeline: ComputePipeline,
      shaderBindings: ShaderLayout,
      descriptorSets: Seq[DescriptorSet],
      dispatchType: DispatchType,
      accessMap: Map[GBinding[?], Operation],
      inputs: Seq[GBinding[?]],
    ) extends PreparedStep

    case class CopyStep(
      source: GBinding[?],
      dest: GBinding[?],
      readIdx: Int,
      writeIdx: Int,
      size: Int,
    ) extends PreparedStep

  /** A wave of dispatches that can execute without barriers between them. */
  case class ExecutionWave(steps: Seq[PreparedStep])

  sealed trait DispatchType
  object DispatchType:
    case class Direct(x: Int, y: Int, z: Int) extends DispatchType
    case class Indirect(buffer: GBinding[?], offset: Int) extends DispatchType

  // === Execution caching ===
  
  /** Key for caching prepared executions. */
  case class ExecutionCacheKey(steps: Seq[StepKey])
  
  sealed trait StepKey
  object StepKey:
    case class ExecuteKey(programName: String, bufferHandles: Seq[Long]) extends StepKey
    case class CopyKey(srcHandle: Long, dstHandle: Long, readIdx: Int, writeIdx: Int, size: Int) extends StepKey
  
  /** Cached prepared execution. */
  case class CachedExecution(preparedSteps: Seq[PreparedStep], waves: Seq[ExecutionWave])
