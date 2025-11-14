package io.computenode.cyfra.runtime

import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Semaphore}
import io.computenode.cyfra.vulkan.core.{Device, Queue}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.vulkan.VK10.{VK_TRUE, vkQueueSubmit}
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

import scala.util.boundary

/** A command buffer that is pending execution, along with its dependencies and cleanup actions.
  *
  * You can call `close()` only when `isFinished || isPending` is true
  *
  * You can call `destroy()` only when all dependants are `isClosed`
  */
class PendingExecution(protected val handle: VkCommandBuffer, val dependencies: Seq[PendingExecution], cleanup: () => Unit, val message: String)(
  using Device,
):
  private var gathered = false
  def isPending: Boolean = !gathered

  private val semaphore: Semaphore = Semaphore()
  def isRunning: Boolean = !isPending && semaphore.isAlive && semaphore.getValue == 0
  def isFinished: Boolean = !semaphore.isAlive || semaphore.getValue > 0
  def block(): Unit = semaphore.waitValue(1)

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
    destroyed = true

  override def toString: String =
    val state = if isPending then "Pending" else if isRunning then "Running" else if isFinished then "Finished" else "Unknown"
    s"PendingExecution($message, $handle, $semaphore, state=$state dependencies=${dependencies.size})"

  /** Gathers all command buffers and their semaphores for submission to the queue, in the correct order.
    *
    * When you call this method, you are expected to submit the command buffers to the queue, and signal the provided semaphore when done.
    *
    * @return
    *   A sequence of tuples, each containing a command buffer, semaphore to signal, and a set of semaphores to wait on.
    */
  private def gatherForSubmission(): Seq[((VkCommandBuffer, Semaphore), Set[Semaphore])] =
    if !isPending then return Seq.empty
    gathered = true
    val mySubmission = ((handle, semaphore), Set.empty[Semaphore])
    dependencies.flatMap(_.gatherForSubmission()).appended(mySubmission)

object PendingExecution:
  def executeAll(executions: Seq[PendingExecution], allocation: VkAllocation)(using Device): Unit = pushStack: stack =>
    assert(executions.forall(_.isPending), "All executions must be pending")
    assert(executions.nonEmpty, "At least one execution must be provided")

    val gathered = executions.flatMap(_.gatherForSubmission()).map(x => (x._1._1, x._1._2, x._2))

    val submitInfos = VkSubmitInfo.calloc(gathered.size, stack)
    gathered.foreach: (commandBuffer, semaphore, dependencies) =>
      val deps = dependencies.toList
      val (semaphores, waitValue, signalValue) = ((semaphore.get, 0L, 1L) +: deps.map(x => (x.get, 1L, 0L))).unzip3

      val timelineSI = VkTimelineSemaphoreSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pWaitSemaphoreValues(stack.longs(waitValue*))
        .pSignalSemaphoreValues(stack.longs(signalValue*))

      submitInfos
        .get()
        .sType$Default()
        .pNext(timelineSI)
        .pCommandBuffers(stack.pointers(commandBuffer, allocation.synchroniseCommand))
        .pSignalSemaphores(stack.longs(semaphores*))
        .pWaitSemaphores(stack.longs(semaphores*))

    submitInfos.flip()

    check(vkQueueSubmit(allocation.commandPool.queue.get, submitInfos, 0), "Failed to submit command buffer to queue")

  def cleanupAll(executions: Seq[PendingExecution]): Unit =
    def cleanupRec(ex: PendingExecution): Unit =
      if !ex.isClosed then return
      ex.close()
      ex.dependencies.foreach(cleanupRec)
    executions.foreach(cleanupRec)
