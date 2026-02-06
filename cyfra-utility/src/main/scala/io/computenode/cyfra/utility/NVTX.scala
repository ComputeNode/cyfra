package io.computenode.cyfra.utility

import com.sun.jna.{Library, Native}

/** NVTX (NVIDIA Tools Extension) wrapper for CPU profiling markers.
  *
  * These markers show up in Nsight Systems timeline alongside Vulkan GPU work.
  *
  * Usage:
  * {{{
  * import io.computenode.cyfra.utility.NVTX
  *
  * NVTX.range("Forward Pass") {
  *   // code to profile
  * }
  *
  * // Or manual push/pop:
  * NVTX.push("Token Generation")
  * // ...
  * NVTX.pop()
  * }}}
  *
  * Requires: CUDA toolkit installed with libnvtx3interop.so in library path.
  * Run with: LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH
  */
object NVTX:

  private trait NVTXLib extends Library:
    def nvtxRangePushA(message: String): Int
    def nvtxRangePop(): Int
    def nvtxMarkA(message: String): Unit

  private lazy val lib: Option[NVTXLib] =
    try
      Some(Native.load("nvtx3interop", classOf[NVTXLib]))
    catch
      case e: UnsatisfiedLinkError =>
        System.err.println(s"[NVTX] Library not found: ${e.getMessage}")
        None

  /** Push a named range onto the NVTX stack. Must be paired with pop(). */
  def push(name: String): Unit =
    lib.foreach(_.nvtxRangePushA(name))

  /** Pop the current range from the NVTX stack. */
  def pop(): Unit =
    lib.foreach(_.nvtxRangePop())

  /** Place an instant marker (point in time, not a range). */
  def mark(name: String): Unit =
    lib.foreach(_.nvtxMarkA(name))

  /** Execute body within a named NVTX range. */
  inline def range[T](name: String)(body: => T): T =
    push(name)
    try body
    finally pop()

  /** Check if NVTX is available. */
  def isAvailable: Boolean = lib.isDefined
