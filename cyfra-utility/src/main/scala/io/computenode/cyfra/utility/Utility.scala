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

  extension (str: String)
    def red: String = Console.RED + str + Console.RESET
    def redb: String = Console.RED_B + str + Console.RESET
    def yellow: String = Console.YELLOW + str + Console.RESET
    def blue: String = Console.BLUE + str + Console.RESET
    def green: String = Console.GREEN + str + Console.RESET

  extension [A](seq: Seq[A])
    def accumulate[B, C](initial: B)(fn: (B, A) => (B, C)): (Seq[C], B) =
      val builder = Seq.newBuilder[C]
      var acc = initial
      for elem <- seq do
        val (nextAcc, res) = fn(acc, elem)
        acc = nextAcc
        builder += res
      (builder.result(), acc)
