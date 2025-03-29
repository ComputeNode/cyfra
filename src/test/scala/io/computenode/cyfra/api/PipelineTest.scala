package io.computenode.cyfra.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.joml.Vector3i
import io.computenode.cyfra.vulkan.compute.Shader.loadShader
import scala.util.{Success, Failure}

class PipelineTest extends AnyFunSuite with BeforeAndAfterEach with Matchers {
  private var context: ComputeContext = _
  
  override def beforeEach(): Unit = {
    context = new ComputeContext(enableValidation = true)
  }
  
  override def afterEach(): Unit = {
    if (context != null) context.close()
  }
  
  test("pipeline creation") {
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
    val pipelineTry = context.createPipeline(shader)
    pipelineTry.isSuccess shouldBe true
    
    val pipeline = pipelineTry.get
    pipeline.getShader should equal(shader)
    
    // Test workgroup calculation
    val workgroupCount = pipeline.calculateWorkgroupCount(1024)
    workgroupCount.x should equal(8) // 1024 / 128 = 8
    workgroupCount.y should equal(1)
    workgroupCount.z should equal(1)
    
    pipeline.close()
    shader.close()
  }
}