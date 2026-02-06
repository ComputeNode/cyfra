package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.inference.LlamaInference
import io.computenode.cyfra.llama.model.LlamaModel
import io.computenode.cyfra.llama.tokenizer.LlamaTokenizer
import io.computenode.cyfra.runtime.VkCyfraRuntime

import java.nio.file.{Files, Paths}

/** Profiling test for UNFUSED F16 pipeline - run with nsys. */
object ProfileUnfusedPipeline:
  def main(args: Array[String]): Unit =
    val modelPath = Paths.get("cyfra-llama/Llama-3.2-1B-Instruct-f16.gguf")
    require(Files.exists(modelPath), s"Model not found: $modelPath")
    
    VkCyfraRuntime.using:
      println("Loading model...")
      val model = LlamaModel.fromGGUF(modelPath)
      val tokenizer = LlamaTokenizer(model.gguf)
      
      try
        println("Creating UNFUSED pipeline...")
        val llamaInference = new LlamaInference(model, maxT = 2048)
        val pipeline = llamaInference.getF16KVCachedPipeline(useFused = false)
        
        val promptTokens = tokenizer.encode("The quick brown fox")
        
        // Warmup
        println("Warmup...")
        pipeline.generate(promptTokens, 10, logits => logits.indices.maxBy(logits))
        
        // Profile run - generate 100 tokens
        println("=== PROFILING START (UNFUSED) ===")
        val startNs = System.nanoTime()
        val generated = pipeline.generate(
          promptTokens = promptTokens,
          maxNewTokens = 100,
          sampleFn = logits => logits.indices.maxBy(logits),
        )
        val elapsedMs = (System.nanoTime() - startNs) / 1_000_000.0
        println("=== PROFILING END ===")
        
        val stats = pipeline.lastStats.get
        println(generated)
        println(f"\nUNFUSED Results:")
        println(f"  Generated: ${generated.length} tokens")
        println(f"  Decode time: ${stats.decodeTimeMs.toInt} ms")
        println(f"  Decode throughput: ${stats.decodeTokPerSec}%.2f tok/s")
        println(f"  Wall time: ${elapsedMs.toInt} ms")
        
      finally
        model.close()
