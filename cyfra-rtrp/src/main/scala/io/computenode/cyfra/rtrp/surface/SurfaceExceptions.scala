package io.computenode.cyfra.rtrp.surface

import io.computenode.cyfra.rtrp.CyfraRtrpException

// Surface system exceptions
sealed abstract class SurfaceSystemException(val message: String, cause: Throwable = null) extends Exception(message, cause) with CyfraRtrpException

case class SurfaceSystemInitializationException(override val message: String, cause: Throwable = null) extends SurfaceSystemException(message, cause)

case class SurfaceSystemShutdownException(override val message: String, cause: Throwable = null) extends SurfaceSystemException(message, cause)

case class SurfaceSystemNotInitializedException(override val message: String = "SurfaceSystem not initialized") extends SurfaceSystemException(message)

// Surface creation/management exceptions
sealed abstract class SurfaceException(val message: String, cause: Throwable = null) extends Exception(message, cause) with CyfraRtrpException

case class SurfaceCreationException(override val message: String, cause: Throwable = null) extends SurfaceException(message, cause)

case class SurfaceDestroyedException(override val message: String = "Surface has been destroyed") extends SurfaceException(message)

case class SurfaceOperationException(override val message: String, cause: Throwable = null) extends SurfaceException(message, cause)

case class SurfaceInvalidException(override val message: String = "Surface is invalid or has been destroyed") extends SurfaceException(message)

case class SurfaceCapabilitiesException(override val message: String, cause: Throwable = null) extends SurfaceException(message, cause)

case class SurfaceResizeException(override val message: String, cause: Throwable = null) extends SurfaceException(message, cause)

case class SurfaceRecreationException(override val message: String, cause: Throwable = null) extends SurfaceException(message, cause)

// Surface configuration exceptions
sealed abstract class SurfaceConfigurationException(val message: String, cause: Throwable = null)
    extends Exception(message, cause)
    with CyfraRtrpException

case class UnsupportedSurfaceFormatException(override val message: String, cause: Throwable = null)
    extends SurfaceConfigurationException(message, cause)

case class UnsupportedPresentModeException(override val message: String, cause: Throwable = null) extends SurfaceConfigurationException(message, cause)

case class InvalidSurfaceExtentException(override val message: String, cause: Throwable = null) extends SurfaceConfigurationException(message, cause)

case class InvalidImageCountException(override val message: String, cause: Throwable = null) extends SurfaceConfigurationException(message, cause)

// Vulkan-specific surface exceptions
sealed abstract class VulkanSurfaceException(val message: String, cause: Throwable = null) extends Exception(message, cause) with CyfraRtrpException

case class VulkanSurfaceCreationException(override val message: String, cause: Throwable = null) extends VulkanSurfaceException(message, cause)

case class VulkanSurfaceCapabilitiesException(override val message: String, cause: Throwable = null) extends VulkanSurfaceException(message, cause)

case class VulkanSurfaceLostException(override val message: String = "Vulkan surface has been lost") extends VulkanSurfaceException(message)

case class VulkanSurfaceOutOfDateException(override val message: String = "Vulkan surface is out of date") extends VulkanSurfaceException(message)

case class VulkanPresentationException(override val message: String, cause: Throwable = null) extends VulkanSurfaceException(message, cause)
