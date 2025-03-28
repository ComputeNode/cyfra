package io.computenode.cyfra.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.joml.Vector3i
import java.nio.ByteBuffer
import io.computenode.cyfra.vulkan.compute.Shader.loadShader
import scala.util.{Success, Failure}

class ShaderTest extends AnyFunSuite with BeforeAndAfterEach with Matchers {
  private var context: ComputeContext = _
  
  override def beforeEach(): Unit = {
    context = new ComputeContext(enableValidation = true)
  }
  
  override def afterEach(): Unit = {
    if (context != null) context.close()
  }
  
  test("shader creation") {
    // This test assumes "copy_test.spv" exists in the resources
    val spirvCode = loadShader("copy_test.spv")
    val layoutInfo = LayoutInfo(Seq(
      LayoutSet(0, Seq(Binding(0, 4))),
      LayoutSet(1, Seq(Binding(0, 4)))
    ))
    
    val shaderTry = context.createShader(
      spirvCode,
      new Vector3i(128, 1, 1),
      layoutInfo
    )
    
    shaderTry.isSuccess shouldBe true
    
    val shader = shaderTry.get
    shader.getWorkgroupDimensions should equal(new Vector3i(128, 1, 1))
    shader.getEntryPoint should equal("main")
    shader.getLayoutInfo.sets.length should equal(2)
    
    shader.close()
  }
  
  test("shader layout info") {
    val spirvCode = loadShader("copy_test.spv")
    val layoutInfo = LayoutInfo.standardIOLayout(1, 1, 4)
    
    val shaderTry = context.createShader(
      spirvCode,
      new Vector3i(128, 1, 1),
      layoutInfo
    )
    
    shaderTry.isSuccess shouldBe true
    
    val shader = shaderTry.get
    shader.getLayoutInfo.sets.size should equal(2)
    shader.getLayoutInfo.sets(0).id should equal(0)
    shader.getLayoutInfo.sets(1).id should equal(1)
    shader.getLayoutInfo.sets(0).bindings.size should equal(1)
    shader.getLayoutInfo.sets(1).bindings.size should equal(1)
    shader.getLayoutInfo.sets(0).bindings(0).size should equal(4)
    
    shader.close()
  }
}