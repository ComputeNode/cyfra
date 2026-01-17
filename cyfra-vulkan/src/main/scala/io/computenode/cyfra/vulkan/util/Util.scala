package io.computenode.cyfra.vulkan.util

import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.VK_SUCCESS

import scala.util.Using
import scala.util.boundary
import scala.util.boundary.Label

object Util:
  def pushStack[T](f: Label[T] ?=> MemoryStack => T): T =
    Using.resource(MemoryStack.stackPush()): stack =>
      boundary:
        f(stack)
  def check(err: Int, message: String = ""): Unit = if err != VK_SUCCESS then throw new VulkanAssertionError(message, err)
