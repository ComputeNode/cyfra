package io.computenode.cyfra.rtrp.window

import io.computenode.cyfra.rtrp.CyfraRtrpException

// Window system exceptions
sealed abstract class WindowSystemException(val message: String, cause: Throwable = null) extends Exception(message, cause) with CyfraRtrpException

case class WindowSystemInitializationException(override val message: String, cause: Throwable = null) extends WindowSystemException(message, cause)

case class WindowSystemShutdownException(override val message: String, cause: Throwable = null) extends WindowSystemException(message, cause)

case class WindowSystemNotInitializedException(override val message: String = "WindowSystem not initialized") extends WindowSystemException(message)

// Window creation/management exceptions
sealed abstract class WindowException(val message: String, cause: Throwable = null) extends Exception(message, cause) with CyfraRtrpException

case class WindowCreationException(override val message: String, cause: Throwable = null) extends WindowException(message, cause)

case class WindowDestroyedException(override val message: String = "Window has been destroyed") extends WindowException(message)

case class WindowOperationException(override val message: String, cause: Throwable = null) extends WindowException(message, cause)

// Platform-specific exceptions
sealed abstract class PlatformException(val message: String, cause: Throwable = null) extends Exception(message, cause) with CyfraRtrpException

case class GLFWException(override val message: String, cause: Throwable = null) extends PlatformException(message, cause)

case class VulkanNotSupportedException(override val message: String = "Vulkan is not supported on this system") extends PlatformException(message)
