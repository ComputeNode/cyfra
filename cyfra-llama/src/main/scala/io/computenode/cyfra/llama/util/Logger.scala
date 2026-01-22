package io.computenode.cyfra.llama.util

import org.slf4j.LoggerFactory

/** Logger for the Llama module using SLF4J. */
object Logger:
  private val logger = LoggerFactory.getLogger("io.computenode.cyfra.llama")
  
  def info(msg: => String): Unit = if logger.isInfoEnabled then logger.info(msg)
  def debug(msg: => String): Unit = if logger.isDebugEnabled then logger.debug(msg)
  def warn(msg: => String): Unit = if logger.isWarnEnabled then logger.warn(msg)
  def error(msg: => String): Unit = if logger.isErrorEnabled then logger.error(msg)
