package io.computenode.cyfra.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import java.nio.ByteBuffer
import org.lwjgl.BufferUtils
import scala.util.{Success, Failure}

class BufferTest extends AnyFunSuite with BeforeAndAfterEach with Matchers {
  private var context: ComputeContext = _
  
  override def beforeEach(): Unit = {
    context = new ComputeContext(enableValidation = true)
  }
  
  override def afterEach(): Unit = {
    if (context != null) context.close()
  }
  
  test("buffer creation") {
    val bufferTry = context.createBuffer(1024, isHostVisible = true)
    bufferTry.isSuccess shouldBe true
    
    val buffer = bufferTry.get
    buffer.getSize should equal(1024)
    buffer.isHostAccessible shouldBe true
    
    buffer.close()
  }
  
  test("buffer copy from and to host") {
    val bufferTry = context.createBuffer(16, isHostVisible = true)
    bufferTry.isSuccess shouldBe true
    
    val buffer = bufferTry.get
    
    // Create test data
    val testData = BufferUtils.createByteBuffer(16)
    for (i <- 0 until 4) {
      testData.putInt(i)
    }
    testData.flip()
    
    // Copy to buffer
    val copyResult = buffer.copyFrom(testData)
    copyResult.isSuccess shouldBe true
    
    // Copy back from buffer
    val resultTry = buffer.copyToHost()
    resultTry.isSuccess shouldBe true
    
    val result = resultTry.get
    result.remaining() should equal(16)
    
    // Verify data
    for (i <- 0 until 4) {
      result.getInt() should equal(i)
    }
    
    buffer.close()
  }
  
  test("buffer duplicate") {
    val bufferTry = context.createBuffer(16, isHostVisible = true)
    bufferTry.isSuccess shouldBe true
    
    val buffer = bufferTry.get
    
    // Create test data
    val testData = BufferUtils.createByteBuffer(16)
    for (i <- 0 until 4) {
      testData.putInt(i)
    }
    testData.flip()
    
    // Copy to buffer
    buffer.copyFrom(testData)
    
    // Duplicate buffer
    val duplicateTry = buffer.duplicate()
    duplicateTry.isSuccess shouldBe true
    
    val duplicate = duplicateTry.get
    
    // Copy back from duplicate
    val resultTry = duplicate.copyToHost()
    resultTry.isSuccess shouldBe true
    
    val result = resultTry.get
    result.remaining() should equal(16)
    
    // Verify data
    for (i <- 0 until 4) {
      result.getInt() should equal(i)
    }
    
    buffer.close()
    duplicate.close()
  }
}