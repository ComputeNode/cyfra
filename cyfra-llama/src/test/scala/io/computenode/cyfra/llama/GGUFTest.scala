package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.model.LlamaModel
import io.computenode.cyfra.llama.gguf.GGUFReader
import munit.FunSuite

import java.nio.file.{Files, Path, Paths}

class GGUFTest extends FunSuite:
  // Set this to the path of a GGUF model file for testing
  val testModelPath: Path = Paths.get(
    sys.env.getOrElse("LLAMA_MODEL_PATH", "models/tinyllama-1.1b-chat-v1.0.Q4_K_M.gguf")
  )

  test("parse GGUF header and metadata"):
    assume(Files.exists(testModelPath), s"Model file not found at $testModelPath")

    val gguf = GGUFReader.read(testModelPath)
    try
      println(s"GGUF Version: ${gguf.version}")
      println(s"Tensors: ${gguf.tensors.size}")
      println(s"Data offset: ${gguf.dataOffset}")
      println()
      
      println("Metadata keys:")
      gguf.metadata.keys.toSeq.sorted.foreach(k => println(s"  $k"))
      println()
      
      println("Architecture:")
      gguf.getString("general.architecture").foreach(v => println(s"  $v"))
      
      println("\nModel parameters:")
      val arch = gguf.getString("general.architecture").getOrElse("llama")
      gguf.getInt(s"$arch.embedding_length").foreach(v => println(s"  embedding_length: $v"))
      gguf.getInt(s"$arch.feed_forward_length").foreach(v => println(s"  feed_forward_length: $v"))
      gguf.getInt(s"$arch.attention.head_count").foreach(v => println(s"  head_count: $v"))
      gguf.getInt(s"$arch.attention.head_count_kv").foreach(v => println(s"  head_count_kv: $v"))
      gguf.getInt(s"$arch.block_count").foreach(v => println(s"  block_count: $v"))
      gguf.getInt(s"$arch.vocab_size").foreach(v => println(s"  vocab_size: $v"))
      gguf.getInt(s"$arch.context_length").foreach(v => println(s"  context_length: $v"))
      println()
      
      println("Tensors (first 30):")
      gguf.tensors.take(30).foreach: t =>
        println(s"  ${t.name}: shape=${t.shape.mkString("x")}, type=${t.quantType}, offset=${t.offset}")

      assert(gguf.tensors.nonEmpty)
    finally
      gguf.close()

  test("load LlamaModel from GGUF"):
    assume(Files.exists(testModelPath), s"Model file not found at $testModelPath")

    val model = LlamaModel.fromGGUF(testModelPath)
    try
      model.logInfo()
      
      // Verify config was extracted correctly
      println(s"\nExtracted config:")
      println(s"  hiddenSize: ${model.config.hiddenSize}")
      println(s"  intermediateSize: ${model.config.intermediateSize}")
      println(s"  numAttentionHeads: ${model.config.numAttentionHeads}")
      println(s"  numKeyValueHeads: ${model.config.numKeyValueHeads}")
      println(s"  numHiddenLayers: ${model.config.numHiddenLayers}")
      println(s"  vocabSize: ${model.config.vocabSize}")
      println(s"  maxPositionEmbeddings: ${model.config.maxPositionEmbeddings}")
      println(s"  headSize: ${model.config.headSize}")
      println(s"  gqaRatio: ${model.config.gqaRatio}")
      println(s"  ropeTheta: ${model.config.ropeTheta}")
      
      // Verify expected tensor names exist
      val expectedTensors = Seq(
        LlamaModel.TensorNames.tokenEmbed,
        LlamaModel.TensorNames.outputNorm,
        LlamaModel.TensorNames.attnNorm(0),
        LlamaModel.TensorNames.attnQ(0),
        LlamaModel.TensorNames.ffnNorm(0),
        LlamaModel.TensorNames.ffnGate(0),
      )
      
      println(s"\nChecking expected tensors:")
      expectedTensors.foreach: name =>
        model.getTensor(name) match
          case Some(t) => println(s"  ✓ $name: ${t.shape.mkString("x")} (${t.quantType})")
          case None => println(s"  ✗ $name: NOT FOUND")
      
      assert(model.config.hiddenSize > 0)
      assert(model.config.numHiddenLayers > 0)
    finally
      model.close()
