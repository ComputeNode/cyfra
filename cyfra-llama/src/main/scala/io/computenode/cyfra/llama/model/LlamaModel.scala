package io.computenode.cyfra.llama.model

import io.computenode.cyfra.llama.gguf.GGUFReader
import io.computenode.cyfra.llama.gguf.GGUFReader.*
import io.computenode.cyfra.llama.util.Logger
import java.nio.file.Path

/** Llama model loaded from GGUF file.
  * 
  * Contains model configuration and weight tensors.
  */
case class LlamaModel(
  config: LlamaConfig,
  gguf: GGUFFile,
):
  /** Get a weight tensor by name. */
  def getTensor(name: String): Option[TensorInfo] = gguf.getTensor(name)

  /** Read a weight tensor as Float32 array (only for F32 tensors). */
  def readWeightF32(name: String): Array[Float] =
    gguf.getTensor(name) match
      case Some(tensor) => gguf.readTensorF32(tensor)
      case None => throw new IllegalArgumentException(s"Tensor not found: $name")

  /** Read raw tensor bytes (for quantized tensors). */
  def readWeightBytes(name: String): Array[Byte] =
    gguf.getTensor(name) match
      case Some(tensor) => gguf.readTensorBytes(tensor)
      case None => throw new IllegalArgumentException(s"Tensor not found: $name")

  /** Close the underlying file. */
  def close(): Unit = gguf.close()

  /** List all tensor names in the model. */
  def tensorNames: Seq[String] = gguf.tensors.map(_.name)

  /** Get model architecture name. */
  def architecture: String = gguf.getString("general.architecture").getOrElse("unknown")

  /** Get model name. */
  def name: String = gguf.getString("general.name").getOrElse("unknown")

  /** Log model info at INFO level. */
  def logInfo(): Unit =
    Logger.info(s"Model: $name, arch=$architecture, ${gguf.tensors.size} tensors")
    Logger.info(s"Config: ${config.hiddenSize}d, ${config.numHiddenLayers}L, ${config.numAttentionHeads}H, vocab=${config.vocabSize}")

object LlamaModel:
  /** Load Llama model from GGUF file.
    * 
    * Extracts model configuration from GGUF metadata.
    */
  def fromGGUF(path: Path): LlamaModel =
    val gguf = GGUFReader.read(path)

    // Extract architecture-specific metadata prefix
    val arch = gguf.getString("general.architecture").getOrElse("llama")

    // Extract model configuration from metadata
    val config = LlamaConfig(
      hiddenSize = gguf.getInt(s"$arch.embedding_length").getOrElse(4096),
      intermediateSize = gguf.getInt(s"$arch.feed_forward_length").getOrElse(11008),
      numAttentionHeads = gguf.getInt(s"$arch.attention.head_count").getOrElse(32),
      numKeyValueHeads = gguf.getInt(s"$arch.attention.head_count_kv").getOrElse(32),
      numHiddenLayers = gguf.getInt(s"$arch.block_count").getOrElse(32),
      vocabSize = gguf.getInt(s"$arch.vocab_size").getOrElse(32000),
      maxPositionEmbeddings = gguf.getInt(s"$arch.context_length").getOrElse(2048),
      rmsNormEps = gguf.getFloat(s"$arch.attention.layer_norm_rms_epsilon").getOrElse(1e-6f),
      ropeTheta = gguf.getFloat(s"$arch.rope.freq_base").getOrElse(10000.0f),
    )

    LlamaModel(config, gguf)

  /** Common Llama tensor name patterns. */
  object TensorNames:
    def tokenEmbed: String = "token_embd.weight"
    def outputNorm: String = "output_norm.weight"
    def output: String = "output.weight"
    
    def attnNorm(layer: Int): String = s"blk.$layer.attn_norm.weight"
    def attnQ(layer: Int): String = s"blk.$layer.attn_q.weight"
    def attnK(layer: Int): String = s"blk.$layer.attn_k.weight"
    def attnV(layer: Int): String = s"blk.$layer.attn_v.weight"
    def attnOutput(layer: Int): String = s"blk.$layer.attn_output.weight"
    
    def ffnNorm(layer: Int): String = s"blk.$layer.ffn_norm.weight"
    def ffnGate(layer: Int): String = s"blk.$layer.ffn_gate.weight"
    def ffnUp(layer: Int): String = s"blk.$layer.ffn_up.weight"
    def ffnDown(layer: Int): String = s"blk.$layer.ffn_down.weight"
