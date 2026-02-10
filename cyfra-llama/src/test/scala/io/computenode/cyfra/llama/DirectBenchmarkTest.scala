package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.gguf.GGUFReader
import io.computenode.cyfra.llama.inference.LlamaInference
import io.computenode.cyfra.llama.tokenizer.LlamaTokenizer
import io.computenode.cyfra.llama.model.LlamaModel
import io.computenode.cyfra.llama.pipeline.LlamaF16Pipeline
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import java.nio.file.{Files, Paths}
import scala.concurrent.duration.*

/** Direct benchmark to verify which code path is actually running. */
class DirectBenchmarkTest extends FunSuite:
  
  val modelPath = "cyfra-llama/Llama-3.2-1B-Instruct-f16.gguf"
  
  override def munitTimeout: Duration = 10.minutes
  
  test("Direct F16 Pipeline.generate benchmark"):
    assume(Files.exists(Paths.get(modelPath)), s"Model not found: $modelPath")
    
    VkCyfraRuntime.using:
      println("Loading model...")
      val model = LlamaModel.fromGGUF(Paths.get(modelPath))
      
      try
        println("Creating inference...")
        val inference = new LlamaInference(model, maxT = 2048)
        val f16Pipeline = inference.getF16Pipeline
        
        val tokenizer = LlamaTokenizer(model.gguf)
        val promptText = "Once upon a time"
        val promptTokens = tokenizer.encode(promptText)
        
        println("\n" + "=" * 60)
        println("  Llama 3.2 1B F16 - KV Cache Benchmark (Cyfra GPU)")
        println("=" * 60)
        
        // Warmup - 3 generations to ensure everything is compiled and cached
        println("\nWarming up (3 generations)...")
        for i <- 1 to 3 do
          f16Pipeline.generate(promptTokens, 20, temperature = 0.0f)
          println(s"  warmup $i done")
        
        // Benchmark with longer generation
        val maxTokens = 128
        println(s"\n--- Benchmark: $maxTokens tokens ---")
        println(s"Prompt: '$promptText'\n")
        
        // Timed generation with output
        print("Output: ")
        val start = System.nanoTime()
        val generated = f16Pipeline.generate(
          promptTokens = promptTokens,
          maxNewTokens = maxTokens,
          temperature = 0.0f, // greedy
          onToken = token => print(tokenizer.decodeToken(token)),
          stopTokens = Set(tokenizer.eosToken, 128009),
        )
        val elapsed = (System.nanoTime() - start) / 1e6
        println("\n")
        
        val tokPerSec = generated.length * 1000.0 / elapsed
        println(s"Generated: ${generated.length} tokens")
        println(s"Time: ${elapsed.toInt} ms")
        println(f"Throughput: $tokPerSec%.1f tok/s")
        
        // Multiple runs for consistency
        println(s"\n--- Consistency check (5 runs x $maxTokens tokens) ---")
        val times = (1 to 5).map: i =>
          val runStart = System.nanoTime()
          val tokens = f16Pipeline.generate(promptTokens, maxTokens, temperature = 0.0f)
          val runElapsed = (System.nanoTime() - runStart) / 1e6
          val runTokPerSec = tokens.length * 1000.0 / runElapsed
          println(f"  Run $i: ${tokens.length} tokens in ${runElapsed.toInt}%5d ms = $runTokPerSec%.1f tok/s")
          runElapsed
        
        val avgTime = times.sum / times.length
        val avgTokPerSec = maxTokens * 1000.0 / avgTime
        val minTime = times.min
        val maxTokPerSec = maxTokens * 1000.0 / minTime
        
        println("\n" + "=" * 60)
        println(f"  Average: $avgTokPerSec%.1f tok/s")
        println(f"  Best:    $maxTokPerSec%.1f tok/s")
        println("=" * 60)
        
      finally
        model.close()
