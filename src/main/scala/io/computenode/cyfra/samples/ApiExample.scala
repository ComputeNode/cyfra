package io.computenode.cyfra.examples

import io.computenode.cyfra.api.*
import org.joml.Vector3i
import org.lwjgl.BufferUtils
import io.computenode.cyfra.vulkan.compute.Shader.loadShader
import scala.util.{Success, Failure, Try}

object SimpleExample {
  def main(args: Array[String]): Unit = {
    // Create a compute context
    val context = new ComputeContext(enableValidation = true)
    
    try {
      // Create an array of integers 0-1023
      val inputData = Array.tabulate(1024)(i => i)
      
      // Create input buffer with the data
      context.createIntBuffer(inputData, isHostVisible = true) match {
        case Success(inputBuffer) =>
          // Create output buffer
          context.createBuffer(4 * 1024) match {
            case Success(outputBuffer) =>
              // Create shader and pipeline
              val spirvCode = loadShader("copy_test.spv")
              val layoutInfo = LayoutInfo.standardIOLayout(1, 1, 4)
              
              for {
                shader <- context.createShader(spirvCode, new Vector3i(128, 1, 1), layoutInfo)
                pipeline <- context.createPipeline(shader)
                _ <- context.execute(pipeline, Seq(inputBuffer), Seq(outputBuffer), 1024)
                resultBuffer <- outputBuffer.copyToHost()
              } yield {
                // Read back and print results
                val results = Array.fill(1024)(0)
                for (i <- 0 until 1024) {
                  results(i) = resultBuffer.getInt()
                }
                
                println("Results: " + results.take(10).mkString(", "))
                
                // Cleanup
                pipeline.close()
                shader.close()
                inputBuffer.close()
                outputBuffer.close()
              }
              
            case Failure(e) => 
              println(s"Failed to create output buffer: ${e.getMessage}")
          }
        
        case Failure(e) => 
          println(s"Failed to create input buffer: ${e.getMessage}")
      }
    } finally {
      context.close()
    }
  }
}