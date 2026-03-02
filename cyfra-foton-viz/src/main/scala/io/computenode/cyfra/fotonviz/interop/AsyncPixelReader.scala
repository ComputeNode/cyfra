package io.computenode.cyfra.fotonviz.interop

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL21.GL_PIXEL_PACK_BUFFER
import org.lwjgl.opengl.GL30.{glMapBufferRange, GL_MAP_READ_BIT}
import org.lwjgl.opengl.GL45.*
import org.lwjgl.system.MemoryUtil.NULL

import java.nio.ByteBuffer

/** Async pixel reader using OpenGL Pixel Buffer Objects (PBOs).
  *
  * Uses double-buffered PBOs to overlap GPU→CPU transfers with rendering.
  * While frame N is being transferred, frame N-1 is being displayed.
  * 
  * This provides ~2x speedup for the transfer phase compared to synchronous reads.
  */
class AsyncPixelReader(val width: Int, val height: Int) extends AutoCloseable:
  
  private val bufferSize = width * height * 4 // RGBA8
  
  // Double-buffered PBOs
  private val pbo = Array.fill(2)(glGenBuffers())
  private var currentPbo = 0
  
  // Initialize PBOs
  pbo.foreach: p =>
    glBindBuffer(GL_PIXEL_PACK_BUFFER, p)
    glBufferData(GL_PIXEL_PACK_BUFFER, bufferSize.toLong, GL_STREAM_READ)
  glBindBuffer(GL_PIXEL_PACK_BUFFER, 0)
  
  private var hasData = false
  private var mappedBuffer: ByteBuffer = null
  
  /** Start async read of pixels from the currently bound framebuffer.
    * Returns pixels from the PREVIOUS frame (or null on first call).
    */
  def readPixelsAsync(): ByteBuffer =
    // Swap PBOs
    val readPbo = pbo(currentPbo)
    val writePbo = pbo(1 - currentPbo)
    currentPbo = 1 - currentPbo
    
    // Start async read into write PBO
    glBindBuffer(GL_PIXEL_PACK_BUFFER, writePbo)
    glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, 0L)
    
    // Read from read PBO (previous frame's data)
    val result = if hasData then
      glBindBuffer(GL_PIXEL_PACK_BUFFER, readPbo)
      // Map for reading
      val mapped = glMapBufferRange(
        GL_PIXEL_PACK_BUFFER,
        0,
        bufferSize.toLong,
        GL_MAP_READ_BIT,
      )
      mappedBuffer = mapped
      mapped
    else
      null
    
    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0)
    hasData = true
    result
  
  /** Unmap the buffer after using the pixels */
  def unmapBuffer(): Unit =
    if mappedBuffer != null then
      val readPbo = pbo(1 - currentPbo) // The one we read from
      glBindBuffer(GL_PIXEL_PACK_BUFFER, readPbo)
      glUnmapBuffer(GL_PIXEL_PACK_BUFFER)
      glBindBuffer(GL_PIXEL_PACK_BUFFER, 0)
      mappedBuffer = null
  
  /** Get the last read buffer (must be called between readPixelsAsync and unmapBuffer) */
  def getBuffer: ByteBuffer = mappedBuffer
  
  override def close(): Unit =
    if mappedBuffer != null then unmapBuffer()
    pbo.foreach(glDeleteBuffers(_))
