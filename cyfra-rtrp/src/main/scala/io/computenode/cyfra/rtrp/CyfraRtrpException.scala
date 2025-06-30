package io.computenode.cyfra.rtrp

// Root exception type for all RTRP-related exceptions
trait CyfraRtrpException extends Exception:
  def message: String
  override def getMessage: String = message
