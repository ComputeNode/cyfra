package io.computenode.cyfra.spirvtools

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}
import scala.io.Source

object SpirvTestUtils {
  def loadShaderFromResources(path: String): ByteBuffer = {
    val resourceUrl = getClass.getClassLoader.getResource(path)
    require(resourceUrl != null, s"Resource not found: $path")
    val bytes = Files.readAllBytes(Paths.get(resourceUrl.toURI))
    ByteBuffer.wrap(bytes)
  }

  def loadResourceAsString(path: String): String = {
    val source = Source.fromResource(path)
    try source.mkString
    finally source.close()
  }

  def corruptMagicNumber(original: ByteBuffer): ByteBuffer = {
    val corrupted = ByteBuffer.allocate(original.capacity())
    original.rewind()
    corrupted.put(original)
    corrupted.rewind()
    corrupted.put(0, 0.toByte)

    corrupted.rewind()
    corrupted
  }
}
