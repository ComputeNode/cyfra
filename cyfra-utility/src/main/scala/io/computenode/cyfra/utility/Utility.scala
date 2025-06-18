package io.computenode.cyfra.utility

import io.computenode.cyfra.utility.Logger.logger

object Utility:

  def timed[T](tag: String = "Time taken")(fn: => T): T =
    val start = System.currentTimeMillis()
    val res = fn
    val end = System.currentTimeMillis()
    logger.debug(s"$tag: ${end - start}ms")
    res
