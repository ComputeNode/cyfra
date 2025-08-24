package io.computenode.cyfra.runtime

import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Semaphore}
import io.computenode.cyfra.vulkan.core.{Device, Queue}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObject
import org.lwjgl.vulkan.VK13.{VK_PIPELINE_STAGE_2_COPY_BIT, vkQueueSubmit2}
import org.lwjgl.vulkan.{VkCommandBuffer, VkCommandBufferSubmitInfo, VkSemaphoreSubmitInfo, VkSubmitInfo2}

import scala.collection.mutable

class PendingExecution(protected val handle: VkCommandBuffer, val waitStage: Long, val dependencies: Seq[PendingExecution], cleanup: () => Unit)(
  using Device,
) extends VulkanObject[VkCommandBuffer]:
  private val semaphore: Semaphore = Semaphore(waitStage)
  private var fence: Option[Fence] = None

  override protected def close(): Unit = cleanup()

  private def setFence(otherFence: Fence): Unit =
    if fence.isDefined then return
    fence = Some(otherFence)
    dependencies.foreach(_.setFence(otherFence))

  private def gatherForSubmission: Seq[((VkCommandBuffer, Semaphore), Set[Semaphore])] = {
    if fence.isDefined then return Seq.empty
    val mySubmission = ((handle, semaphore), dependencies.map(_.semaphore).toSet)
    dependencies.flatMap(_.gatherForSubmission).appended(mySubmission)
  }

  def block(): Unit =
    fence match
      case Some(f) => f.block()
      case None    => throw new IllegalStateException("No fence set for this execution")

object PendingExecution:
  def executeAll(executions: Seq[PendingExecution], queue: Queue)(using Device): Fence = pushStack: stack =>
    val gathered = executions.flatMap(_.gatherForSubmission).toSet.groupMap(_._2)(_._1).toSeq

    val submitInfos = VkSubmitInfo2.calloc(gathered.size, stack)
    gathered.foreach: (semaphores, executions) =>
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
          .stageMask(s.stage)

      pCommandBuffersSI.flip()
      signalSemaphoreSI.flip()

      val waitSemaphoreSI = VkSemaphoreSubmitInfo.calloc(semaphores.size, stack)
      semaphores.foreach: s =>
        waitSemaphoreSI
          .get()
          .sType$Default()
          .semaphore(s.get)
          .stageMask(s.stage)

      waitSemaphoreSI.flip()

      submitInfos
        .get()
        .sType$Default()
        .flags(0)
        .pCommandBufferInfos(pCommandBuffersSI)
        .pSignalSemaphoreInfos(signalSemaphoreSI)
        .pWaitSemaphoreInfos(waitSemaphoreSI)

    submitInfos.flip()

    val fence = Fence()
    executions.foreach(_.setFence(fence))
    check(vkQueueSubmit2(queue.get, submitInfos, fence.get), "Failed to submit command buffer to queue")
    fence

  def cleanupAll(executions: Seq[PendingExecution]): Unit =
    def cleanupRec(ex: PendingExecution): Unit =
      if !ex.isAlive then return
      ex.destroy()
      ex.dependencies.foreach(cleanupRec)
    executions.foreach(cleanupRec)
