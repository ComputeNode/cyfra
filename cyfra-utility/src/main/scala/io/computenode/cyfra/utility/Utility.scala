package io.computenode.cyfra.utility

import io.computenode.cyfra.utility.Logger.logger

import java.util.concurrent.atomic.AtomicInteger

object Utility:

  def timed[T](tag: String = "Time taken")(fn: => T): T =
    val start = System.currentTimeMillis()
    val res = fn
    val end = System.currentTimeMillis()
    logger.debug(s"$tag: ${end - start}ms")
    res

  private val aint = AtomicInteger(0)
  def nextId(): Int = aint.getAndIncrement()
