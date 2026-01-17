package io.computenode.cyfra.runtime

import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Semaphore}
import io.computenode.cyfra.vulkan.core.{Device, Queue}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK13.{VK_PIPELINE_STAGE_2_COPY_BIT, vkQueueSubmit2}
import org.lwjgl.vulkan.{
  VK13,
  VkCommandBuffer,
  VkCommandBufferSubmitInfo,
  VkSemaphoreSubmitInfo,
  VkSubmitInfo,
  VkSubmitInfo2,
  VkTimelineSemaphoreSubmitInfo,
}

import scala.util.boundary.break

/** A command buffer that is pending execution, along with its dependencies and cleanup actions.
  *
  * You can call `close()` only when `isFinished || isPending` is true
  *
  * You can call `destroy()` only when all dependants are `isClosed`
  */
class PendingExecution(protected val handle: VkCommandBuffer, val dependencies: Seq[PendingExecution], cleanup: () => Unit)(using Device):
  private var fence: Option[Fence] = None
  def isPending: Boolean = fence.isEmpty
  def isRunning: Boolean = fence.exists(f => f.isAlive && !f.isSignaled)
  def isFinished: Boolean = fence.exists(f => !f.isAlive || f.isSignaled)
  def block(): Unit = fence.foreach(f => if f.isAlive then f.block())

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
    fence.foreach(x => if x.isAlive then x.destroy())
    destroyed = true

  override def toString: String =
    val state = if isPending then "Pending" else if isRunning then "Running" else if isFinished then "Finished" else "Unknown"
    s"PendingExecution($handle, ${fence.getOrElse("")}, state=$state dependencies=${dependencies.size})"

  /** Gathers all command buffers and their semaphores for submission to the queue, in the correct order.
    *
    * When you call this method, you are expected to submit the command buffers to the queue, and signal the provided semaphore when done.
    *
    * @return
    *   A sequence of tuples, each containing a command buffer, semaphore to signal, and a set of semaphores to wait on.
    */
  private def gatherForSubmission(f: Fence): Seq[VkCommandBuffer] =
    if !isPending then return Seq.empty
    fence = Some(f)
    dependencies.flatMap(_.gatherForSubmission(f)).appended(handle)

object PendingExecution:
  def executeAll(executions: Seq[PendingExecution], allocation: VkAllocation)(using Device): Unit = pushStack: stack =>
    assert(executions.forall(_.isPending), "All executions must be pending")
    if executions.isEmpty then break()

    val fence = Fence()
    val gathered = executions.flatMap(_.gatherForSubmission(fence))

    val submitInfos = VkSubmitInfo.calloc(gathered.size, stack)
    gathered.foreach: commandBuffer =>
      submitInfos
        .get()
        .sType$Default()
        .pCommandBuffers(stack.pointers(commandBuffer, allocation.synchroniseCommand))
    submitInfos.flip()

    check(vkQueueSubmit(allocation.commandPool.queue.get, submitInfos, fence.get), "Failed to submit command buffer to queue")

  def cleanupAll(executions: Seq[PendingExecution]): Unit =
    def cleanupRec(ex: PendingExecution): Unit =
      if ex.isClosed then return
      ex.close()
      ex.dependencies.foreach(cleanupRec)
    executions.foreach(cleanupRec)
