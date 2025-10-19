package io.computenode.cyfra.runtime

import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Semaphore}
import io.computenode.cyfra.vulkan.core.{Device, Queue}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.vulkan.VK10.VK_TRUE
import org.lwjgl.vulkan.VK13.{VK_PIPELINE_STAGE_2_COPY_BIT, vkQueueSubmit2}
import org.lwjgl.vulkan.{VK13, VkCommandBuffer, VkCommandBufferSubmitInfo, VkSemaphoreSubmitInfo, VkSubmitInfo2}

import scala.collection.mutable

/** A command buffer that is pending execution, along with its dependencies and cleanup actions.
  *
  * You can call `close()` only when `isFinished || isPending` is true
  *
  * You can call `destroy()` only when all dependants are `isClosed`
  */
class PendingExecution(protected val handle: VkCommandBuffer, val dependencies: Seq[PendingExecution], cleanup: () => Unit)(using Device):
  private val semaphore: Semaphore = Semaphore()
  private var fence: Option[Fence] = None

  def isPending: Boolean = fence.isEmpty
  def isRunning: Boolean = fence.exists(f => f.isAlive && !f.isSignaled)
  def isFinished: Boolean = fence.exists(f => !f.isAlive || f.isSignaled)

  def block(): Unit = fence.foreach(_.block())

  private var closed = false
  def isClosed: Boolean = closed
  private def close(): Unit =
    assert(isFinished || isPending, "Cannot close a PendingExecution that is not finished or pending")
    if closed then return
    cleanup()
    closed = true

  private var destroyed = false
  def destroy(): Unit =
    if destroyed then return
    close()
    semaphore.destroy()
    fence.foreach(x => if x.isAlive then x.destroy())
    destroyed = true

  /** Gathers all command buffers and their semaphores for submission to the queue, in the correct order.
    *
    * When you call this method, you are expected to submit the command buffers to the queue, and signal the provided fence when done.
    * @param f
    *   The fence to signal when the command buffers are done executing.
    * @return
    *   A sequence of tuples, each containing a command buffer, semaphore to signal, and a set of semaphores to wait on.
    */
  private def gatherForSubmission(f: Fence): Seq[((VkCommandBuffer, Semaphore), Set[Semaphore])] =
    if !isPending then return Seq.empty
    val mySubmission = ((handle, semaphore), dependencies.map(_.semaphore).toSet)
    fence = Some(f)
    dependencies.flatMap(_.gatherForSubmission(f)).appended(mySubmission)

object PendingExecution:
  def executeAll(executions: Seq[PendingExecution], queue: Queue)(using Device): Fence = pushStack: stack =>
    assert(executions.forall(_.isPending), "All executions must be pending")
    assert(executions.nonEmpty, "At least one execution must be provided")

    val fence = Fence()

    val exec: Seq[(Set[Semaphore], Set[(VkCommandBuffer, Semaphore)])] =
      val gathered = executions.flatMap(_.gatherForSubmission(fence))
      val ordering = gathered.zipWithIndex.map(x => (x._1._1._1, x._2)).toMap
      gathered.toSet.groupMap(_._2)(_._1).toSeq.sortBy(x => x._2.map(_._1).map(ordering).min)

    val submitInfos = VkSubmitInfo2.calloc(exec.size, stack)
    exec.foreach: (semaphores, executions) =>
      val pCommandBuffersSI = VkCommandBufferSubmitInfo.calloc(executions.size, stack)
      val signalSemaphoreSI = VkSemaphoreSubmitInfo.calloc(executions.size, stack)
      executions.foreach: (cb, s) =>
        pCommandBuffersSI
          .get()
          .sType$Default()
          .commandBuffer(cb)
          .deviceMask(0)
        signalSemaphoreSI
          .get()
          .sType$Default()
          .semaphore(s.get)
          .stageMask(VK13.VK_PIPELINE_STAGE_2_COPY_BIT | VK13.VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)

      pCommandBuffersSI.flip()
      signalSemaphoreSI.flip()

      val waitSemaphoreSI = VkSemaphoreSubmitInfo.calloc(semaphores.size, stack)
      semaphores.foreach: s =>
        waitSemaphoreSI
          .get()
          .sType$Default()
          .semaphore(s.get)
          .stageMask(VK13.VK_PIPELINE_STAGE_2_COPY_BIT | VK13.VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT)

      waitSemaphoreSI.flip()

      submitInfos
        .get()
        .sType$Default()
        .flags(0)
        .pCommandBufferInfos(pCommandBuffersSI)
        .pSignalSemaphoreInfos(signalSemaphoreSI)
        .pWaitSemaphoreInfos(waitSemaphoreSI)

    submitInfos.flip()

    check(vkQueueSubmit2(queue.get, submitInfos, fence.get), "Failed to submit command buffer to queue")
    fence

  def cleanupAll(executions: Seq[PendingExecution]): Unit =
    def cleanupRec(ex: PendingExecution): Unit =
      if !ex.isClosed then return
      ex.close()
      ex.dependencies.foreach(cleanupRec)
    executions.foreach(cleanupRec)
