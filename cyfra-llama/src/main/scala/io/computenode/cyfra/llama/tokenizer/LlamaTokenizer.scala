package io.computenode.cyfra.llama.tokenizer

import io.computenode.cyfra.llama.gguf.GGUFReader.GGUFFile
import scala.collection.mutable

/** Simple BPE tokenizer for Llama models.
  * 
  * Reads vocabulary from GGUF metadata.
  * Supports encoding (text -> tokens) and decoding (tokens -> text).
  */
class LlamaTokenizer(gguf: GGUFFile):
  
  // Special tokens
  val bosToken: Int = gguf.getInt("tokenizer.ggml.bos_token_id").getOrElse(1)
  val eosToken: Int = gguf.getInt("tokenizer.ggml.eos_token_id").getOrElse(2)
  val padToken: Int = gguf.getInt("tokenizer.ggml.padding_token_id").getOrElse(0)
  
  // Load vocabulary from GGUF
  private val vocab: Array[String] = gguf.getStringArray("tokenizer.ggml.tokens").getOrElse(Array.empty)
  private val scores: Array[Float] = gguf.getFloatArray("tokenizer.ggml.scores").getOrElse(Array.empty)
  
  // Build reverse lookup for encoding
  private val tokenToId: Map[String, Int] = vocab.zipWithIndex.toMap
  
  /** Number of tokens in vocabulary. */
  def vocabSize: Int = vocab.length
  
  // GPT-style special characters used by Llama 3
  private val GPT_SPACE = '\u0120'  // Ġ - space marker
  private val GPT_NEWLINE = '\u010A' // Ċ - newline marker  
  private val GPT_TAB = '\u0109'    // ĉ - tab marker
  
  /** Decode a single token to string.
    * Handles special byte tokens like <0xNN> and GPT-style markers.
    */
  def decodeToken(tokenId: Int): String =
    if tokenId < 0 || tokenId >= vocab.length then
      s"<UNK:$tokenId>"
    else
      val token = vocab(tokenId)
      // Handle byte tokens like <0xNN>
      if token.startsWith("<0x") && token.endsWith(">") then
        try
          val byteVal = Integer.parseInt(token.drop(3).dropRight(1), 16)
          new String(Array(byteVal.toByte), "UTF-8")
        catch
          case _: Exception => token
      else
        // Replace GPT-style markers with actual characters
        token
          .replace(GPT_SPACE.toString, " ")
          .replace(GPT_NEWLINE.toString, "\n")
          .replace(GPT_TAB.toString, "\t")
          .replace("▁", " ")  // Sentencepiece space marker
  
  /** Decode a sequence of tokens to string. */
  def decode(tokens: Array[Int]): String =
    tokens.map(decodeToken).mkString
  
  // Detect which space marker the vocabulary uses (▁ for Llama 1/2, Ġ for Llama 3)
  private val spaceMarker: String =
    if tokenToId.contains("Ġ") || vocab.exists(_.startsWith("Ġ")) then "Ġ"
    else "▁"
  
  /** Encode text to tokens using greedy longest-match BPE.
    * 
    * This handles both SentencePiece (▁) and GPT (Ġ) space marker conventions:
    * - Space marker represents a space before the token
    * - First token of a word has space marker prefix
    * 
    * Note: This is a simplified implementation. For production,
    * use the official sentencepiece tokenizer.
    */
  def encode(text: String, addBos: Boolean = true): Array[Int] =
    val tokens = mutable.ArrayBuffer[Int]()
    
    if addBos then
      tokens += bosToken
    
    // Replace spaces with detected space marker and prepend for start
    val normalized = spaceMarker + text.replace(" ", spaceMarker)
    
    // Greedy longest-match tokenization
    var pos = 0
    while pos < normalized.length do
      var found = false
      var maxLen = math.min(normalized.length - pos, 64)  // Max token length
      
      // Try to find longest matching token
      while maxLen > 0 && !found do
        val candidate = normalized.substring(pos, pos + maxLen)
        if tokenToId.contains(candidate) then
          tokens += tokenToId(candidate)
          pos += maxLen
          found = true
        else
          maxLen -= 1
      
      if !found then
        // Single character fallback
        val char = normalized.charAt(pos)
        val charStr = char.toString
        if tokenToId.contains(charStr) then
          tokens += tokenToId(charStr)
        else
          // Unknown character - try byte fallback for UTF-8 bytes
          val bytes = charStr.getBytes("UTF-8")
          for b <- bytes do
            val byteToken = f"<0x${b & 0xFF}%02X>"
            tokenToId.get(byteToken).foreach(tokens += _)
        pos += 1
    
    tokens.toArray
  
  /** Get token string by ID (for debugging). */
  def getToken(tokenId: Int): String =
    if tokenId >= 0 && tokenId < vocab.length then vocab(tokenId)
    else s"<UNK:$tokenId>"

object LlamaTokenizer:
  def apply(gguf: GGUFFile): LlamaTokenizer = new LlamaTokenizer(gguf)
