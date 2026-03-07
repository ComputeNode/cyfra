package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.inference.LlamaInference
import io.computenode.cyfra.llama.model.LlamaModel
import io.computenode.cyfra.llama.pipeline.LlamaPipeline
import io.computenode.cyfra.llama.tokenizer.LlamaTokenizer
import io.computenode.cyfra.llama.util.{Console, Logger}
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
    prependInference: String = "",  // Text to prepend to AI responses (included in context)
    maxTokens: Int = 500,
    contextSize: Int = 2048,
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
    
    println()
    printHeader()
    println()
    println(Console.title("▸ Cyfra Llama Runner"))
    println(Console.statLine("Model:", config.modelPath))
    println(Console.statLine("Type: ", resolvedType))
    
    VkCyfraRuntime.using:
      val model = LlamaModel.fromGGUF(Paths.get(config.modelPath))
      val tokenizer = LlamaTokenizer(model.gguf)
      val inference = new LlamaInference(model, maxT = config.contextSize)
      
      val pipeline: LlamaPipeline = resolvedType match
        case "f16" => inference.getF16Pipeline
        case _ =>
          System.err.println(s"Unknown model type: $resolvedType")
          return
      
      println(Console.muted(s"Ready: ${model.config.hiddenSize}d, ${model.config.numHiddenLayers}L"))
      println()
      
      if config.measure then
        runBenchmark(pipeline, tokenizer, config)
      else if config.interactive then
        runInteractive(pipeline, tokenizer, config)
      else if config.prompt.isDefined then
        runOnce(pipeline, tokenizer, config.prompt.get, config)
      else
        printUsage()

  private def runInteractive(pipeline: LlamaPipeline, tokenizer: LlamaTokenizer, config: Config): Unit =
    // Quick GPU warm-up
    print(Console.dim("Warming up GPU... "))
    System.out.flush()
    val warmupTokens = tokenizer.encode("There once was a king that")
    pipeline.generate(warmupTokens, maxNewTokens = 40, temperature = 0f, topP = 1f, stopTokens = Set(tokenizer.eosToken))
    println(Console.dim("ready"))
    println()
    
    println(Console.subtitle("Interactive mode") + Console.dim(" · type 'quit' to exit"))
    println(Console.divider())
    
    var running = true
    while running do
      print(s"\n${Console.prompt("You:")} ")
      System.out.flush()
      val userInput = StdIn.readLine()
      
      if userInput == null || userInput.trim.toLowerCase == "quit" || userInput.trim.toLowerCase == "exit" then
        println(Console.dim("\nGoodbye."))
        running = false
      else if userInput.trim.nonEmpty then
        val prepend = config.prependInference
        val prompt = s"<|user|>\n${userInput.trim}</s>\n<|assistant|>\n$prepend"
        runGeneration(pipeline, tokenizer, prompt, config, if prepend.nonEmpty then Some(prepend) else None)

  private def runOnce(pipeline: LlamaPipeline, tokenizer: LlamaTokenizer, prompt: String, config: Config): Unit =
    println(Console.statLine("Prompt:", prompt))
    println()
    runGeneration(pipeline, tokenizer, prompt, config)

  private def runBenchmark(pipeline: LlamaPipeline, tokenizer: LlamaTokenizer, config: Config): Unit =
    val prompt = config.prompt.getOrElse("Once upon a time")
    val tokens = tokenizer.encode(prompt)
    
    println(Console.subtitle("Benchmark"))
    println(Console.statLine("Prompt:", s"'$prompt' → ${config.maxTokens} tokens"))
    println(Console.statLine("Runs:  ", s"${config.warmupRuns} warmup, ${config.benchmarkRuns} measured"))
    println(Console.muted(f"  Sampling: temp=${config.temperature}%.2f, top_p=${config.topP}%.2f"))
    println()
    
    // Warmup
    print(Console.dim("Warming up: "))
    for i <- 1 to config.warmupRuns do
      pipeline.generate(
        tokens, config.maxTokens,
        temperature = config.temperature,
        topP = config.topP,
        stopTokens = Set(tokenizer.eosToken),
      )
      print(Console.dim(s"$i "))
      System.out.flush()
    println(Console.dim("done"))
    println()
    
    // Benchmark runs
    println(Console.subtitle("Results"))
    
    val (decoded, stats, lastGenerated) = (1 to config.benchmarkRuns).map: i =>
      val generated = pipeline.generate(
        tokens, config.maxTokens,
        temperature = config.temperature,
        topP = config.topP,
        stopTokens = Set(tokenizer.eosToken),
      )
      val decoded = tokenizer.decode(generated)
      val s = pipeline.lastStats.get
      println(Console.muted(f"  Run $i:") + f" ${s.generatedTokens} tokens, " + Console.accent(f"${s.decodeTokPerSec}%.1f tok/s"))
      (decoded, s, generated)
    .unzip3
    
    val avgDecode = stats.map(_.decodeTokPerSec).sum / stats.length
    val bestDecode = stats.map(_.decodeTokPerSec).max

    println()
    println(Console.divider())
    println(Console.dim("Last output: ") + decoded.last.toString.take(80) + Console.dim("..."))
    println()
    println(Console.stat("Average:", f"$avgDecode%.1f tok/s"))
    println(Console.stat("Best:   ", f"$bestDecode%.1f tok/s"))

  private def runGeneration(
    pipeline: LlamaPipeline,
    tokenizer: LlamaTokenizer,
    prompt: String,
    config: Config,
    hint: Option[String] = None,
  ): Unit =
    val tokens = tokenizer.encode(prompt)
    
    if config.batch then
      // Batch mode: print at end
      val generated = pipeline.generate(
        promptTokens = tokens,
        maxNewTokens = config.maxTokens,
        temperature = config.temperature,
        topP = config.topP,
        stopTokens = Set(tokenizer.eosToken),
      )
      val decoded = tokenizer.decode(generated)
      val fullResponse = hint.getOrElse("") + decoded
      println(Console.subtitle("AI:") + s" $fullResponse")
    else
      // Streaming mode: print tokens as they arrive
      print(Console.subtitle("AI:") + " " + hint.getOrElse(""))
      System.out.flush()
      val generated = pipeline.generate(
        promptTokens = tokens,
        maxNewTokens = config.maxTokens,
        temperature = config.temperature,
        topP = config.topP,
        onToken = token =>
          val text = tokenizer.decodeToken(token)
          if !text.contains("</s>") && !text.contains("<|") then
            print(text)
            System.out.flush()
        ,
        stopTokens = Set(tokenizer.eosToken),
      )
      println()
      pipeline.lastStats match
        case Some(stats) =>
          println(Console.dim(f"   ${generated.length} tokens · ${stats.decodeTokPerSec}%.1f tok/s"))
        case None =>
          println(Console.dim(f"   ${generated.length} tokens"))

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
        case "--prepend-inference" if i + 1 < args.length =>
          config = config.copy(prependInference = args(i + 1))
          i += 2
        case "--max-tokens" | "-n" if i + 1 < args.length =>
          config = config.copy(maxTokens = args(i + 1).toInt)
          i += 2
        case "--ctx-size" | "-c" if i + 1 < args.length =>
          config = config.copy(contextSize = args(i + 1).toInt)
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

  private val Header: String =
    """              ▄▄                   ▄▄ ▄▄                      
      |             ██                    ██ ██                      
      |▄████ ██ ██ ▀██▀ ████▄  ▀▀█▄       ██ ██  ▀▀█▄ ███▄███▄  ▀▀█▄ 
      |██    ██▄██  ██  ██ ▀▀ ▄█▀██ ▀▀▀▀▀ ██ ██ ▄█▀██ ██ ██ ██ ▄█▀██ 
      |▀████  ▀██▀  ██  ██    ▀█▄██       ██ ██ ▀█▄██ ██ ██ ██ ▀█▄██ 
      |        ██                                                    
      |      ▀▀▀                                                     """.stripMargin

  private def printHeader(): Unit =
    Header.linesIterator.foreach(line => println(Console.title(line)))
    println()

  private def printUsage(): Unit =
    println()
    printHeader()
    println(s"  ${Console.muted("Usage:")} runner [OPTIONS] [MODEL_PATH]")
    println()
    println(Console.subtitle("  Modes"))
    println(s"    ${Console.accent("-i")}, ${Console.accent("--interactive")}     Interactive chat mode")
    println(s"    ${Console.accent("-p")}, ${Console.accent("--prompt")} TEXT     Single prompt and exit")
    println(s"    ${Console.accent("--measure")}             Benchmark mode")
    println()
    println(Console.subtitle("  Options"))
    println(s"    ${Console.accent("-m")}, ${Console.accent("--model")} PATH      Path to GGUF model file")
    println(s"    ${Console.accent("-t")}, ${Console.accent("--type")} TYPE       Model type: f16, f32, auto ${Console.dim("(default: auto)")}")
    println(s"    ${Console.accent("-b")}, ${Console.accent("--batch")}           Buffer output, print at end")
    println(s"    ${Console.accent("--prepend-inference")} TEXT   Prepend text to AI responses")
    println(s"    ${Console.accent("-n")}, ${Console.accent("--max-tokens")} N    Maximum tokens ${Console.dim("(default: 500)")}")
    println(s"    ${Console.accent("-c")}, ${Console.accent("--ctx-size")} N      Context size ${Console.dim("(default: 2048)")}")
    println(s"    ${Console.accent("--temperature")} FLOAT   Sampling temperature ${Console.dim("(default: 0.7)")}")
    println(s"    ${Console.accent("--top-p")} FLOAT         Top-p threshold ${Console.dim("(default: 0.9)")}")
    println()
    println(Console.subtitle("  Examples"))
    println(Console.dim("    runner -m model.gguf -t f16 -i"))
    println(Console.dim("    runner -m model.gguf -p \"Hello world\" -n 100"))
    println(Console.dim("    runner -m model.gguf --measure -n 128"))
    println()

