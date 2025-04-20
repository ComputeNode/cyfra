package io.computenode.cyfra.utility

object Utility:

  def timed[T](tag: String = "Time taken")(fn: => T): T = 
    val start = System.currentTimeMillis()
    val res = fn
    val end = System.currentTimeMillis()
    println(s"$tag: ${end - start}ms")
    res
