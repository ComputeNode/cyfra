package io.computenode.cyfra.vulkan.executor

import org.lwjgl.vulkan.VK10.{VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_BUFFER_USAGE_TRANSFER_SRC_BIT}

enum BufferAction(val action: Int):
  case DoNothing extends BufferAction(0)
  case LoadTo extends BufferAction(VK_BUFFER_USAGE_TRANSFER_DST_BIT)
  case LoadFrom extends BufferAction(VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
  case LoadFromTo extends BufferAction(VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT)

  private def findAction(action: Int): BufferAction = action match
    case VK_BUFFER_USAGE_TRANSFER_DST_BIT => LoadTo
    case VK_BUFFER_USAGE_TRANSFER_SRC_BIT => LoadFrom
    case 3                                => LoadFromTo
    case _                                => DoNothing

  def |(other: BufferAction): BufferAction = findAction(this.action | other.action)
