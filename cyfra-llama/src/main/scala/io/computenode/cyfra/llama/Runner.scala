package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.inference.LlamaInference
import io.computenode.cyfra.llama.model.LlamaModel
import io.computenode.cyfra.llama.pipeline.LlamaPipeline
import io.computenode.cyfra.llama.tokenizer.LlamaTokenizer
import io.computenode.cyfra.llama.util.Logger
import io.computenode.cyfra.runtime.VkCyfraRuntime

import java.nio.file.{Files, Paths}
import scala.io.StdIn

/** Llama model runner with F16 and F32 pipeline support.
  *
  * Usage:
  *   runner --model path/to/model.gguf --type f16 --interactive
  *   runner --model path/to/model.gguf --type f32 --prompt "Hello world"
  */
object Runner:

  case class Config(
    modelPath: String = "",
    modelType: String = "auto",  // "f16", "f32", or "auto"
    interactive: Boolean = false,
    measure: Boolean = false,
    batch: Boolean = false,  // Buffer output, print at end
    prompt: Option[String] = None,
    maxTokens: Int = 500,
    temperature: Float = 0.7f,
    topP: Float = 0.9f,
    warmupRuns: Int = 3,
    benchmarkRuns: Int = 5,
  )

  def main(args: Array[String]): Unit =
    val config = parseArgs(args)
    
    if config.modelPath.isEmpty then
      printUsage()
      return
    
    if !Files.exists(Paths.get(config.modelPath)) then
      System.err.println(s"Error: Model not found: ${config.modelPath}")
      return
    
    val resolvedType = if config.modelType == "auto" then
      if config.modelPath.toLowerCase.contains("f16") then "f16" else "f32"
    else config.modelType
    
    println(s"Cyfra Llama Runner")
    println(s"Model: ${config.modelPath}")
    println(s"Type: $resolvedType")
    
    VkCyfraRuntime.using:
      val model = LlamaModel.fromGGUF(Paths.get(config.modelPath))
      val tokenizer = LlamaTokenizer(model.gguf)
      val useQuantized = resolvedType == "f32"
      val inference = new LlamaInference(model, maxT = 1024, useQuantized = useQuantized)
      
      val pipeline: LlamaPipeline = resolvedType match
        case "f16" => inference.getF16KVCachedPipeline
        case "f32" => inference.getF32KVCachedPipeline
        case _ =>
          System.err.println(s"Unknown model type: $resolvedType")
          return
      
      println(s"Ready: ${model.config.hiddenSize}d, ${model.config.numHiddenLayers}L\n")
      
      if config.measure then
        runBenchmark(pipeline, tokenizer, config)
      else if config.interactive then
        runInteractive(pipeline, tokenizer, config)
      else if config.prompt.isDefined then
        runOnce(pipeline, tokenizer, config.prompt.get, config)
      else
        printUsage()

  private def runInteractive(pipeline: LlamaPipeline, tokenizer: LlamaTokenizer, config: Config): Unit =
    println("Interactive mode. Commands: quit, exit")
    println("-" * 40)
    
    var running = true
    while running do
      print("\nYou: ")
      System.out.flush()
      val userInput = StdIn.readLine()
      
      if userInput == null || userInput.trim.toLowerCase == "quit" || userInput.trim.toLowerCase == "exit" then
        running = false
      else if userInput.trim.nonEmpty then
        val prompt = s"<|user|>\n${userInput.trim}</s>\n<|assistant|>\n"
        runGeneration(pipeline, tokenizer, prompt, config)

  private def runOnce(pipeline: LlamaPipeline, tokenizer: LlamaTokenizer, prompt: String, config: Config): Unit =
    println(s"Prompt: $prompt\n")
    runGeneration(pipeline, tokenizer, prompt, config)

  private def runBenchmark(pipeline: LlamaPipeline, tokenizer: LlamaTokenizer, config: Config): Unit =
    val prompt = config.prompt.getOrElse("Once upon a time")
    val tokens = tokenizer.encode(prompt)
    
    println(s"Benchmark: '$prompt' -> ${config.maxTokens} tokens")
    println(s"Warmup: ${config.warmupRuns} runs, Benchmark: ${config.benchmarkRuns} runs\n")
    
    // Greedy argmax sampling
    def argmax(logits: Array[Float]): Int =
      var maxIdx = 0
      var maxVal = logits(0)
      var i = 1
      while i < logits.length do
        if logits(i) > maxVal then
          maxVal = logits(i)
          maxIdx = i
        i += 1
      maxIdx
    
    // Warmup
    print("Warming up: ")
    for i <- 1 to config.warmupRuns do
      pipeline.generate(tokens, config.maxTokens, argmax, _ => (), Set(tokenizer.eosToken), reportStats = false)
      print(s"$i ")
      System.out.flush()
    println("done\n")
    
    // Benchmark runs
    println("Benchmark runs:")
    
    val (decoded, stats) = (1 to config.benchmarkRuns).map: i =>
      val generated = pipeline.generate(tokens, config.maxTokens, argmax, _ => (), Set(tokenizer.eosToken), reportStats = false)
      val decoded = tokenizer.decode(generated)
      val s = pipeline.lastStats.get
      println(f"  Run $i: ${s.generatedTokens} tokens, generate ${s.decodeTokPerSec}%.1f tok/s")
      (decoded, s)
    .unzip
    
    val avgDecode = stats.map(_.decodeTokPerSec).sum / stats.length
    val bestDecode = stats.map(_.decodeTokPerSec).max
    
    println()
    println("Last generation:")
    println(decoded.last.toString)
    println(f"Average: $avgDecode%.1f tok/s")
    println(f"Best:    $bestDecode%.1f tok/s")

  private def runGeneration(pipeline: LlamaPipeline, tokenizer: LlamaTokenizer, prompt: String, config: Config): Unit =
    val tokens = tokenizer.encode(prompt)
    
    if config.batch then
      // Batch mode: print at end
      val buffer = new StringBuilder()
      val generated = pipeline.generate(
        promptTokens = tokens,
        maxNewTokens = config.maxTokens,
        sampleFn = logits => topPSample(logits, config.temperature, config.topP),
        onToken = _ => (),
        stopTokens = Set(tokenizer.eosToken),
        reportStats = false,
      )
      val decoded = tokenizer.decode(generated)
      println(s"Output: $decoded")
    else
      // Streaming mode: print tokens as they arrive
      print("Output: ")
      System.out.flush()
      val generated = pipeline.generate(
        promptTokens = tokens,
        maxNewTokens = config.maxTokens,
        sampleFn = logits => topPSample(logits, config.temperature, config.topP),
        onToken = token =>
          val text = tokenizer.decodeToken(token)
          if !text.contains("</s>") && !text.contains("<|") then
            print(text)
            System.out.flush()
        ,
        stopTokens = Set(tokenizer.eosToken),
        reportStats = false,
      )
      println()
      pipeline.lastStats match
        case Some(stats) =>
          println(f"[${generated.length} tokens, generate ${stats.decodeTokPerSec}%.1f tok/s]")
        case None =>
          println(f"[${generated.length} tokens]")

  private def parseArgs(args: Array[String]): Config =
    var config = Config()
    var i = 0
    while i < args.length do
      args(i) match
        case "--model" | "-m" if i + 1 < args.length =>
          config = config.copy(modelPath = args(i + 1))
          i += 2
        case "--type" | "-t" if i + 1 < args.length =>
          config = config.copy(modelType = args(i + 1).toLowerCase)
          i += 2
        case "--interactive" | "-i" =>
          config = config.copy(interactive = true)
          i += 1
        case "--measure" =>
          config = config.copy(measure = true)
          i += 1
        case "--batch" | "-b" =>
          config = config.copy(batch = true)
          i += 1
        case "--warmup" if i + 1 < args.length =>
          config = config.copy(warmupRuns = args(i + 1).toInt)
          i += 2
        case "--runs" if i + 1 < args.length =>
          config = config.copy(benchmarkRuns = args(i + 1).toInt)
          i += 2
        case "--prompt" | "-p" if i + 1 < args.length =>
          config = config.copy(prompt = Some(args(i + 1)))
          i += 2
        case "--max-tokens" | "-n" if i + 1 < args.length =>
          config = config.copy(maxTokens = args(i + 1).toInt)
          i += 2
        case "--temperature" if i + 1 < args.length =>
          config = config.copy(temperature = args(i + 1).toFloat)
          i += 2
        case "--top-p" if i + 1 < args.length =>
          config = config.copy(topP = args(i + 1).toFloat)
          i += 2
        case arg if !arg.startsWith("-") && config.modelPath.isEmpty =>
          config = config.copy(modelPath = arg)
          i += 1
        case other =>
          System.err.println(s"Unknown argument: $other")
          i += 1
    config

  private def printUsage(): Unit =
    println("""
      |Usage: runner [OPTIONS] [MODEL_PATH]
      |
      |Modes:
      |  -i, --interactive     Interactive chat mode (streaming output)
      |  -p, --prompt TEXT     Single prompt and exit (streaming output)
      |  --measure             Benchmark mode (no output, multiple runs)
      |
      |Options:
      |  -m, --model PATH      Path to GGUF model file
      |  -t, --type TYPE       Model type: f16, f32, or auto (default: auto)
      |  -b, --batch           Buffer output, print at end (faster)
      |  -n, --max-tokens N    Maximum tokens to generate (default: 500)
      |  --temperature FLOAT   Sampling temperature (default: 0.7)
      |  --top-p FLOAT         Top-p sampling threshold (default: 0.9)
      |  --warmup N            Warmup runs for benchmark (default: 3)
      |  --runs N              Benchmark runs (default: 5)
      |
      |Examples:
      |  runner -m model.gguf -t f16 -i
      |  runner -m model.gguf -p "Hello world" -b -n 100
      |  runner -m model.gguf --measure -n 128
      |""".stripMargin)

  private def topPSample(logits: Array[Float], temperature: Float, topP: Float): Int =
    val scaled = logits.map(_ / temperature)
    val maxLogit = scaled.max
    val expLogits = scaled.map(x => math.exp(x - maxLogit).toFloat)
    val sumExp = expLogits.sum
    val probs = expLogits.map(_ / sumExp)
    val indexed = probs.zipWithIndex.sortBy(-_._1)
    
    var cumSum = 0.0f
    var cutoffIdx = 0
    while cutoffIdx < indexed.length && cumSum < topP do
      cumSum += indexed(cutoffIdx)._1
      cutoffIdx += 1
    
    val topTokens = indexed.take(cutoffIdx)
    val topSum = topTokens.map(_._1).sum
    val normalized = topTokens.map(t => (t._1 / topSum, t._2))
    
    val r = scala.util.Random.nextFloat()
    var acc = 0.0f
    var result = normalized.last._2
    for (prob, idx) <- normalized do
      acc += prob
      if acc >= r && result == normalized.last._2 then
        result = idx
    result
