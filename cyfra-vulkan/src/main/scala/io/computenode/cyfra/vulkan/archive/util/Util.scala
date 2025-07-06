package io.computenode.cyfra.vulkan.archive.util

import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.VK_SUCCESS

import scala.util.Using

object Util:
  def pushStack[T](f: MemoryStack => T): T = Using(MemoryStack.stackPush())(f).get
  def check(err: Int, message: String = ""): Unit = if err != VK_SUCCESS then throw new VulkanAssertionError(message, err)
