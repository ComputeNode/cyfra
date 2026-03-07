package io.computenode.cyfra.fotonviz.audio

import javax.sound.sampled.*
import scala.util.Try

/**
 * Audio capture with CPU-based FFT analysis.
 * Computes frequency band levels on CPU and exposes them for GPU visualization.
 *
 * @param sampleRate Audio sample rate (default 44100 Hz)
 * @param bufferSize FFT buffer size (512 = 11.6ms latency, 86Hz resolution)
 * @param smoothingFactor Exponential smoothing (unused, we use attack/release)
 */
class AudioCapture(
  val sampleRate: Int = 44100,
  val bufferSize: Int = 512,  // 512/44100 = 11.6ms, binSize = 86Hz (good for bass)
  val smoothingFactor: Float = 0.7f,
):
  import AudioCapture.*

  private var targetDataLine: Option[TargetDataLine] = None
  private var captureThread: Option[Thread] = None
  @volatile private var running: Boolean = false

  // Audio sample buffer
  private val samples = new Array[Float](bufferSize)
  
  // FFT working arrays
  private val fftReal = new Array[Float](bufferSize)
  private val fftImag = new Array[Float](bufferSize)
  private val magnitudes = new Array[Float](bufferSize / 2)
  
  // Hanning window
  private val window = Array.tabulate(bufferSize)(i =>
    (0.5f * (1.0f - math.cos(2.0 * math.Pi * i / (bufferSize - 1)))).toFloat
  )

  // Frequency band levels (smoothed)
  @volatile private var _bass: Float = 0f
  @volatile private var _lowMid: Float = 0f
  @volatile private var _highMid: Float = 0f
  @volatile private var _treble: Float = 0f
  @volatile private var _peakLevel: Float = 0f

  /** Bass level (20-250 Hz), 0-1 range */
  def bass: Float = _bass
  /** Low-mid level (250-1000 Hz), 0-1 range */
  def lowMid: Float = _lowMid
  /** High-mid level (1000-4000 Hz), 0-1 range */
  def highMid: Float = _highMid
  /** Treble level (4000-16000 Hz), 0-1 range */
  def treble: Float = _treble
  /** Peak audio level, 0-1 range */
  def peakLevel: Float = _peakLevel

  def start(mixerName: Option[String] = None): Boolean =
    if running then return true

    val format = new AudioFormat(sampleRate.toFloat, 16, 1, true, false)
    val dataLineInfo = new DataLine.Info(classOf[TargetDataLine], format)

    try
      val line = mixerName match
        case Some(name) =>
          findMixerByName(name, dataLineInfo).getOrElse:
            println(s"Available audio inputs: ${listAudioInputs.mkString(", ")}")
            throw new IllegalArgumentException(s"Mixer '$name' not found")
        case None =>
          AudioSystem.getLine(dataLineInfo).asInstanceOf[TargetDataLine]

      line.open(format, bufferSize * 4)
      line.start()
      targetDataLine = Some(line)

      running = true
      captureThread = Some(Thread(() => captureLoop(line), "AudioCapture"))
      captureThread.foreach: t =>
        t.setDaemon(true)
        t.start()

      println(s"AudioCapture started: ${line.getLineInfo}")
      true
    catch
      case e: Exception =>
        println(s"Failed to start audio capture: ${e.getMessage}")
        e.printStackTrace()
        false

  def stop(): Unit =
    running = false
    captureThread.foreach(_.join(1000))
    targetDataLine.foreach: line =>
      line.stop()
      line.close()
    targetDataLine = None
    captureThread = None
    println("AudioCapture stopped")

  def isRunning: Boolean = running

  private var debugCounter = 0

  private def captureLoop(line: TargetDataLine): Unit =
    val audioBuffer = new Array[Byte](bufferSize * 2)

    while running do
      val bytesRead = line.read(audioBuffer, 0, audioBuffer.length)
      if bytesRead > 0 then
        // Convert bytes to float samples
        var i = 0
        var maxSample = 0.0f
        while i < bufferSize && (i * 2 + 1) < bytesRead do
          val low = audioBuffer(i * 2) & 0xFF
          val high = audioBuffer(i * 2 + 1)
          val sample = (high << 8) | low
          val normalized = sample / 32768.0f
          samples(i) = normalized
          maxSample = math.max(maxSample, math.abs(normalized))
          i += 1

        _peakLevel = math.max(maxSample, _peakLevel * 0.95f)

        // Compute FFT and extract bands
        computeFFT()
        extractBands()

        // Debug output every ~1 second (44100/512 ≈ 86 buffers/sec)
        debugCounter += 1
        println(f"Audio: bass=${_bass * 100}%.0f%% mid=${_lowMid * 100}%.0f%% hi=${_highMid * 100}%.0f%% treble=${_treble * 100}%.0f%%")

  private def computeFFT(): Unit =
    // Apply window and copy to FFT arrays
    var i = 0
    while i < bufferSize do
      fftReal(i) = samples(i) * window(i)
      fftImag(i) = 0f
      i += 1

    // In-place Cooley-Tukey FFT
    fft(fftReal, fftImag)

    // Compute magnitudes
    i = 0
    val halfSize = bufferSize / 2
    while i < halfSize do
      val re = fftReal(i)
      val im = fftImag(i)
      magnitudes(i) = math.sqrt(re * re + im * im).toFloat / bufferSize
      i += 1

  private def extractBands(): Unit =
    val binSize = sampleRate.toFloat / bufferSize

    def bandPower(lowHz: Float, highHz: Float, gain: Float): Float =
      val startBin = math.max(1, (lowHz / binSize).toInt)
      val endBin = math.min(magnitudes.length - 1, (highHz / binSize).toInt)
      var sum = 0f
      var i = startBin
      while i <= endBin do
        sum += magnitudes(i)
        i += 1
      val avg = if endBin > startBin then sum / (endBin - startBin) else 0f
      math.min(1f, avg * gain)

    // Extract bands - tight ranges for clean dynamics
    val newBass = bandPower(50, 300, 10f)   // Kick drum only, high gain
    val newLowMid = bandPower(300, 600, 70f)  // Bass line
    val newHighMid = bandPower(600, 4000, 90f)
    val newTreble = bandPower(4000, 16000, 130f)

    // Noise gate - cut low-level noise
    def gated(v: Float, threshold: Float): Float =
      if v < threshold then 0f else v
    
    println(f"New bass: ${newBass * 100}%.0f%%")
    val bassGated = gated(newBass, 0.08f)
    println(f"Bass gated: ${bassGated * 100}%.0f%%")
    val midGated = gated(newLowMid, 0.05f)
    val hiMidGated = gated(newHighMid, 0.04f)
    val trebleGated = gated(newTreble, 0.03f)

    // BASS: Very fast attack, VERY slow release (holds when sustained)
    // This keeps waves RAISED during bass hold, with vibration on top
    val bassAttack = 0.5f   
    val bassRelease = 0.1f  
    
    // Other bands: faster release for responsiveness
    val otherAttack = 0.8f
    val otherRelease = 0.3f
    
    def bassEnvelope(current: Float, target: Float): Float =
      if target > current then
        current + (target - current) * bassAttack
      else
        current + (target - current) * bassRelease
        
    def otherEnvelope(current: Float, target: Float): Float =
      if target > current then
        current + (target - current) * otherAttack
      else
        current + (target - current) * otherRelease
    
    _bass = bassEnvelope(_bass, bassGated)
    _lowMid = otherEnvelope(_lowMid, midGated)
    _highMid = otherEnvelope(_highMid, hiMidGated)
    _treble = otherEnvelope(_treble, trebleGated)

  private def fft(real: Array[Float], imag: Array[Float]): Unit =
    val n = real.length
    // Bit-reverse permutation
    var i = 0
    var j = 0
    while i < n do
      if j > i then
        val tr = real(j); val ti = imag(j)
        real(j) = real(i); imag(j) = imag(i)
        real(i) = tr; imag(i) = ti
      var m = n >> 1
      while m >= 2 && j >= m do
        j -= m; m >>= 1
      j += m
      i += 1

    // FFT butterfly
    var step = 1
    while step < n do
      val jump = step << 1
      val delta = math.Pi / step
      i = 0
      while i < step do
        val angle = i * delta
        val wr = math.cos(angle)
        val wi = -math.sin(angle)
        var pair = i
        while pair < n do
          val match_ = pair + step
          val tr = wr * real(match_) - wi * imag(match_)
          val ti = wr * imag(match_) + wi * real(match_)
          real(match_) = (real(pair) - tr).toFloat
          imag(match_) = (imag(pair) - ti).toFloat
          real(pair) = (real(pair) + tr).toFloat
          imag(pair) = (imag(pair) + ti).toFloat
          pair += jump
        i += 1
      step <<= 1

object AudioCapture:
  /** List available audio input devices. */
  def listAudioInputs: Seq[String] =
    AudioSystem.getMixerInfo.flatMap: info =>
      val mixer = AudioSystem.getMixer(info)
      val targetLines = mixer.getTargetLineInfo
      if targetLines.exists(_.getLineClass == classOf[TargetDataLine]) then
        Some(info.getName)
      else
        None
    .toSeq

  /** Find mixer by name (case-insensitive partial match). */
  private def findMixerByName(name: String, lineInfo: DataLine.Info): Option[TargetDataLine] =
    // Debug: show all mixer names
    println(s"Looking for mixer containing: '$name'")
    val allMixers = AudioSystem.getMixerInfo.toSeq
    allMixers.foreach: info =>
      val mixer = AudioSystem.getMixer(info)
      val hasTargetLine = mixer.getTargetLineInfo.exists(_.getLineClass == classOf[TargetDataLine])
      println(s"  Mixer: '${info.getName}' (has input: $hasTargetLine)")

    // Try to find a mixer with a TargetDataLine that matches the name
    allMixers
      .filter(_.getName.toLowerCase.contains(name.toLowerCase))
      .flatMap: info =>
        Try:
          val mixer = AudioSystem.getMixer(info)
          if mixer.getTargetLineInfo.exists(_.getLineClass == classOf[TargetDataLine]) then
            Some(mixer.getLine(lineInfo).asInstanceOf[TargetDataLine])
          else
            None
        .toOption.flatten
      .headOption
