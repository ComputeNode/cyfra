package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.inference.LlamaInference
import io.computenode.cyfra.llama.model.LlamaModel
import io.computenode.cyfra.llama.tokenizer.LlamaTokenizer
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import java.nio.file.{Files, Paths}
import scala.concurrent.duration.*

/** Tests for F16 KV Cache Pipeline with Vec4-optimized matmuls.
  * 
  * This tests LlamaF16Pipeline which provides O(1) per-token inference
  * by maintaining an F16 KV cache on GPU. Uses Vec4 weight loads for 4x bandwidth.
  */
class F16KVCacheTest extends FunSuite:
  
  val modelPath = Paths.get("cyfra-llama/Llama-3.2-1B-Instruct-f16.gguf")
  
  override def munitTimeout: Duration = 15.minutes

  test("F16 KV Cache Pipeline - longer generation benchmark"):
    assume(Files.exists(modelPath), s"Model not found: $modelPath")
    
    VkCyfraRuntime.using:
      println("\n" + "=" * 70)
      println("  F16 KV Cache Pipeline - Performance Benchmark")
      println("=" * 70)
      
      val model = LlamaModel.fromGGUF(modelPath)
      val tokenizer = LlamaTokenizer(model.gguf)
      
      try
        val inference = new LlamaInference(model, maxT = 2048)
        val f16Pipeline = inference.getF16Pipeline
        
        val promptText = "Here is a Python server that creates a new user in the database and the repository:"
        val promptTokens = tokenizer.encode(promptText)
        val maxTokens = 1000
        
        println(s"\nPrompt: '$promptText'")
        println(s"Generating $maxTokens tokens...")
        
        // Warmup
        println("Warming up (1 generation)...")
        f16Pipeline.generate(promptTokens, 100, temperature = 0.2f)

        
        // Benchmark
        println(s"\n--- Benchmark: $maxTokens tokens ---\n")
        print("Output: " + promptText)
        val wallStart = System.nanoTime()
        val generated = f16Pipeline.generate(
          promptTokens = promptTokens,
          maxNewTokens = maxTokens,
          temperature = 0.2f,
          topP = 0.9f,
          onToken = token => print(tokenizer.decodeToken(token)),
          stopTokens = Set(tokenizer.eosToken, 128009), // EOS + end-of-turn
          reportStats = true,  // Print GPU execution timing
        )
        val wallElapsed = (System.nanoTime() - wallStart) / 1e6
        println("\n")
        
        val wallTokPerSec = generated.length * 1000.0 / wallElapsed
        println(f"Wall-clock time: ${wallElapsed.toInt} ms ($wallTokPerSec%.2f tok/s)")
        
        // Multiple runs - GPU-only timing
        println(s"\n--- Consistency check (5 runs x $maxTokens tokens) ---")
        val gpuTimes = (1 to 5).map: i =>
          f16Pipeline.generate(
            promptTokens = promptTokens, 
            maxNewTokens = maxTokens, 
            temperature = 0.2f,
            stopTokens = Set(tokenizer.eosToken, 128009),
          )
          val stats = f16Pipeline.lastStats.get
          println(f"  Run $i: ${stats.generatedTokens} tokens, decode ${stats.decodeTimeMs.toInt}%5d ms = ${stats.decodeTokPerSec}%.2f tok/s (prefill ${stats.prefillTimeMs.toInt} ms)")
          stats.decodeTokPerSec
        
        val avgDecodeTokPerSec = gpuTimes.sum / gpuTimes.length
        
        println("\n" + "=" * 70)
        println(f"  Average decode throughput: $avgDecodeTokPerSec%.2f tok/s")
        println("  Memory usage: ~50% of F32 (F16 weights + F16 KV cache)")
        println("=" * 70)
        
      finally
        model.close()

end F16KVCacheTest
