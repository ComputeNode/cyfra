package io.computenode.cyfra.rtrp.surface.core

import io.computenode.cyfra.rtrp.window.core.{WindowEvent, WindowId}

// Unique id for surfaces
case class SurfaceId(value: Long) extends AnyVal

// Surface format definitions
sealed trait SurfaceFormat {
  def vulkanValue: Int
}

object SurfaceFormat {
  case object B8G8R8A8_SRGB extends SurfaceFormat {
    val vulkanValue = 50
  }
  case object B8G8R8A8_UNORM extends SurfaceFormat {
    val vulkanValue = 44
  }
  case object R8G8B8A8_SRGB extends SurfaceFormat {
    val vulkanValue = 43
  }
  case object R8G8B8A8_UNORM extends SurfaceFormat {
    val vulkanValue = 37
  }
}

// Color space definitions
sealed trait ColorSpace {
  def vulkanValue: Int
}

object ColorSpace {
  case object SRGB_NONLINEAR extends ColorSpace {
    val vulkanValue = 0 // VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  }
  case object DISPLAY_P3_NONLINEAR extends ColorSpace {
    val vulkanValue = 1000104001 // VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
  }
}

// Present mode definitions
sealed trait PresentMode {
  def vulkanValue: Int
}

object PresentMode {
  case object IMMEDIATE extends PresentMode {
    val vulkanValue = 0 // VK_PRESENT_MODE_IMMEDIATE_KHR
  }
  case object MAILBOX extends PresentMode {
    val vulkanValue = 1 // VK_PRESENT_MODE_MAILBOX_KHR
  }
  case object FIFO extends PresentMode {
    val vulkanValue = 2 // VK_PRESENT_MODE_FIFO_KHR
  }
  case object FIFO_RELAXED extends PresentMode {
    val vulkanValue = 3 // VK_PRESENT_MODE_FIFO_RELAXED_KHR
  }
}
