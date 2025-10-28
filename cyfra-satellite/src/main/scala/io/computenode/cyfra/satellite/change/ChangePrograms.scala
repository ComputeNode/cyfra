package io.computenode.cyfra.satellite.change

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.core.*
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** GPU programs for temporal change detection */
object ChangePrograms:
  
  /** Layout for dNBR calculation */
  case class DNBRParams(size: Int)
  
  case class DNBRLayout(
      nbrBefore: GBuffer[Float32],
      nbrAfter: GBuffer[Float32],
      dnbr: GBuffer[Float32],
      severity: GBuffer[Int32]
  ) extends Layout
  
  /** GPU program for dNBR and burn severity classification */
  val dnbrProgram = GProgram[DNBRParams, DNBRLayout](
    layout = params =>
      DNBRLayout(
        nbrBefore = GBuffer[Float32](params.size),
        nbrAfter = GBuffer[Float32](params.size),
        dnbr = GBuffer[Float32](params.size),
        severity = GBuffer[Int32](params.size)
      ),
    dispatch = (_, params) => GProgram.StaticDispatch(((params.size + 127) / 128, 1, 1)),
    workgroupSize = (128, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val before = GIO.read(layout.nbrBefore, idx)
    val after = GIO.read(layout.nbrAfter, idx)
    val dnbrValue = ChangeDetection.differenceNBR(before, after)
    val severityClass = ChangeDetection.classifyBurnSeverity(dnbrValue)
    for
      _ <- GIO.write(layout.dnbr, idx, dnbrValue)
      _ <- GIO.write(layout.severity, idx, severityClass)
    yield ()
  
  /** Layout for comprehensive change detection */
  case class ChangeDetectionParams(size: Int)
  
  case class ChangeDetectionLayout(
      ndviBefore: GBuffer[Float32],
      ndviAfter: GBuffer[Float32],
      nbrBefore: GBuffer[Float32],
      nbrAfter: GBuffer[Float32],
      ndviChange: GBuffer[Float32],
      nbrChange: GBuffer[Float32],
      disturbanceScore: GBuffer[Float32],
      changeType: GBuffer[Int32]
  ) extends Layout
  
  /** GPU program for comprehensive change detection */
  val changeDetectionProgram = GProgram[ChangeDetectionParams, ChangeDetectionLayout](
    layout = params =>
      ChangeDetectionLayout(
        ndviBefore = GBuffer[Float32](params.size),
        ndviAfter = GBuffer[Float32](params.size),
        nbrBefore = GBuffer[Float32](params.size),
        nbrAfter = GBuffer[Float32](params.size),
        ndviChange = GBuffer[Float32](params.size),
        nbrChange = GBuffer[Float32](params.size),
        disturbanceScore = GBuffer[Float32](params.size),
        changeType = GBuffer[Int32](params.size)
      ),
    dispatch = (_, params) => GProgram.StaticDispatch(((params.size + 127) / 128, 1, 1)),
    workgroupSize = (128, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val ndviBef = GIO.read(layout.ndviBefore, idx)
    val ndviAft = GIO.read(layout.ndviAfter, idx)
    val nbrBef = GIO.read(layout.nbrBefore, idx)
    val nbrAft = GIO.read(layout.nbrAfter, idx)
    val ndviChg = ChangeDetection.ndviDrop(ndviBef, ndviAft)
    val nbrChg = ChangeDetection.differenceNBR(nbrBef, nbrAft)
    val distScore = ChangeDetection.disturbanceScore(ndviBef, ndviAft, nbrBef, nbrAft)
    val chgType = ChangeDetection.classifyChangeType(ndviChg, nbrChg)
    for
      _ <- GIO.write(layout.ndviChange, idx, ndviChg)
      _ <- GIO.write(layout.nbrChange, idx, nbrChg)
      _ <- GIO.write(layout.disturbanceScore, idx, distScore)
      _ <- GIO.write(layout.changeType, idx, chgType)
    yield ()
  
  /** Layout for deforestation detection */
  case class DeforestationParams(size: Int)
  
  case class DeforestationLayout(
      ndviBefore: GBuffer[Float32],
      ndviAfter: GBuffer[Float32],
      ndviChange: GBuffer[Float32],
      confidence: GBuffer[Float32]
  ) extends Layout
  
  /** GPU program for deforestation/logging detection */
  val deforestationProgram = GProgram[DeforestationParams, DeforestationLayout](
    layout = params =>
      DeforestationLayout(
        ndviBefore = GBuffer[Float32](params.size),
        ndviAfter = GBuffer[Float32](params.size),
        ndviChange = GBuffer[Float32](params.size),
        confidence = GBuffer[Float32](params.size)
      ),
    dispatch = (_, params) => GProgram.StaticDispatch(((params.size + 127) / 128, 1, 1)),
    workgroupSize = (128, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val before = GIO.read(layout.ndviBefore, idx)
    val after = GIO.read(layout.ndviAfter, idx)
    val change = ChangeDetection.ndviDrop(before, after)
    val conf = ChangeDetection.deforestationConfidence(change, before)
    for
      _ <- GIO.write(layout.ndviChange, idx, change)
      _ <- GIO.write(layout.confidence, idx, conf)
    yield ()

/** High-level API for change detection analysis */
class ChangeAnalyzer(using runtime: VkCyfraRuntime):
  
  // Maximum batch size
  private val MAX_BATCH_SIZE = 1024 * 1024
  
  /** Helper functions from SpectralPrograms */
  private def nextPowerOf2(n: Int): Int =
    if n <= 0 then 1
    else
      var p = 1
      while p < n do p *= 2
      p
  
  private def padArray(data: Array[Float], targetSize: Int): Array[Float] =
    if data.length >= targetSize then data
    else
      val padded = Array.ofDim[Float](targetSize)
      Array.copy(data, 0, padded, 0, data.length)
      padded
  
  /** Compute dNBR for fire detection */
  def computeDNBR(
      nbrBefore: Array[Float],
      nbrAfter: Array[Float],
      width: Int,
      height: Int
  ): (Array[Float], Array[Int]) =
    val pixelCount = width * height
    val batchSize = nextPowerOf2(math.min(pixelCount, MAX_BATCH_SIZE))
    
    val nbrBefPadded = padArray(nbrBefore, batchSize)
    val nbrAftPadded = padArray(nbrAfter, batchSize)
    
    val params = ChangePrograms.DNBRParams(batchSize)
    
    // Create buffers
    val beforeBuf = BufferUtils.createByteBuffer(batchSize * 4)
    beforeBuf.asFloatBuffer().put(nbrBefPadded).flip()
    
    val afterBuf = BufferUtils.createByteBuffer(batchSize * 4)
    afterBuf.asFloatBuffer().put(nbrAftPadded).flip()
    
    val dnbrBuf = BufferUtils.createFloatBuffer(batchSize)
    val dnbrByteBuf = MemoryUtil.memByteBuffer(dnbrBuf)
    
    val severityBuf = BufferUtils.createIntBuffer(batchSize)
    val severityByteBuf = MemoryUtil.memByteBuffer(severityBuf)
    
    // Execute on GPU
    val region = GBufferRegion
      .allocate[ChangePrograms.DNBRLayout]
      .map: region =>
        ChangePrograms.dnbrProgram.execute(params, region)
    
    region.runUnsafe(
      init = ChangePrograms.DNBRLayout(
        nbrBefore = GBuffer[Float32](beforeBuf),
        nbrAfter = GBuffer[Float32](afterBuf),
        dnbr = GBuffer[Float32](batchSize),
        severity = GBuffer[Int32](batchSize)
      ),
      onDone = layout =>
        layout.dnbr.read(dnbrByteBuf)
        layout.severity.read(severityByteBuf)
    )
    
    // Copy results
    val dnbr = Array.ofDim[Float](pixelCount)
    val severity = Array.ofDim[Int](pixelCount)
    dnbrBuf.get(dnbr)
    severityBuf.get(severity)
    
    (dnbr, severity)
  
  /** Comprehensive change detection (logging and fires) */
  def detectChanges(
      imageBefore: SatelliteImage,
      imageAfter: SatelliteImage
  ): ChangeResult =
    require(imageBefore.width == imageAfter.width && imageBefore.height == imageAfter.height,
      "Images must have same dimensions")
    require(imageBefore.hasBand("NIR") && imageAfter.hasBand("NIR"), "Images must have NIR band")
    require(imageBefore.hasBand("SWIR") && imageAfter.hasBand("SWIR"), "Images must have SWIR band")
    
    val pixelCount = imageBefore.pixelCount
    val batchSize = nextPowerOf2(math.min(pixelCount, MAX_BATCH_SIZE))
    
    // Pad arrays
    val ndviBeforePadded = padArray(computeNDVIArray(imageBefore), batchSize)
    val ndviAfterPadded = padArray(computeNDVIArray(imageAfter), batchSize)
    val nbrBeforePadded = padArray(computeNBRArray(imageBefore), batchSize)
    val nbrAfterPadded = padArray(computeNBRArray(imageAfter), batchSize)
    
    val params = ChangePrograms.ChangeDetectionParams(batchSize)
    
    // Create buffers
    val ndviBefBuf = BufferUtils.createByteBuffer(batchSize * 4)
    ndviBefBuf.asFloatBuffer().put(ndviBeforePadded).flip()
    
    val ndviAftBuf = BufferUtils.createByteBuffer(batchSize * 4)
    ndviAftBuf.asFloatBuffer().put(ndviAfterPadded).flip()
    
    val nbrBefBuf = BufferUtils.createByteBuffer(batchSize * 4)
    nbrBefBuf.asFloatBuffer().put(nbrBeforePadded).flip()
    
    val nbrAftBuf = BufferUtils.createByteBuffer(batchSize * 4)
    nbrAftBuf.asFloatBuffer().put(nbrAfterPadded).flip()
    
    val ndviChgBuf = BufferUtils.createFloatBuffer(batchSize)
    val ndviChgByteBuf = MemoryUtil.memByteBuffer(ndviChgBuf)
    
    val nbrChgBuf = BufferUtils.createFloatBuffer(batchSize)
    val nbrChgByteBuf = MemoryUtil.memByteBuffer(nbrChgBuf)
    
    val distBuf = BufferUtils.createFloatBuffer(batchSize)
    val distByteBuf = MemoryUtil.memByteBuffer(distBuf)
    
    val typeBuf = BufferUtils.createIntBuffer(batchSize)
    val typeByteBuf = MemoryUtil.memByteBuffer(typeBuf)
    
    // Execute on GPU
    val region = GBufferRegion
      .allocate[ChangePrograms.ChangeDetectionLayout]
      .map: region =>
        ChangePrograms.changeDetectionProgram.execute(params, region)
    
    region.runUnsafe(
      init = ChangePrograms.ChangeDetectionLayout(
        ndviBefore = GBuffer[Float32](ndviBefBuf),
        ndviAfter = GBuffer[Float32](ndviAftBuf),
        nbrBefore = GBuffer[Float32](nbrBefBuf),
        nbrAfter = GBuffer[Float32](nbrAftBuf),
        ndviChange = GBuffer[Float32](batchSize),
        nbrChange = GBuffer[Float32](batchSize),
        disturbanceScore = GBuffer[Float32](batchSize),
        changeType = GBuffer[Int32](batchSize)
      ),
      onDone = layout =>
        layout.ndviChange.read(ndviChgByteBuf)
        layout.nbrChange.read(nbrChgByteBuf)
        layout.disturbanceScore.read(distByteBuf)
        layout.changeType.read(typeByteBuf)
    )
    
    // Copy results
    val ndviChange = Array.ofDim[Float](pixelCount)
    val nbrChange = Array.ofDim[Float](pixelCount)
    val disturbanceScore = Array.ofDim[Float](pixelCount)
    val changeType = Array.ofDim[Int](pixelCount)
    
    ndviChgBuf.get(ndviChange)
    nbrChgBuf.get(nbrChange)
    distBuf.get(disturbanceScore)
    typeBuf.get(changeType)
    
    ChangeResult(
      ndviChange,
      nbrChange,
      disturbanceScore,
      changeType,
      imageBefore.width,
      imageBefore.height
    )
  
  /** Helper: Compute NDVI array from image */
  private def computeNDVIArray(image: SatelliteImage): Array[Float] =
    val nir = image.bands("NIR")
    val red = image.bands("Red")
    nir.zip(red).map { case (n, r) =>
      val denom = n + r
      if math.abs(denom) < 0.0001f then 0.0f
      else (n - r) / denom
    }
  
  /** Helper: Compute NBR array from image */
  private def computeNBRArray(image: SatelliteImage): Array[Float] =
    val nir = image.bands("NIR")
    val swir = image.bands("SWIR")
    nir.zip(swir).map { case (n, s) =>
      val denom = n + s
      if math.abs(denom) < 0.0001f then 0.0f
      else (n - s) / denom
    }

/** Result of change detection analysis */
case class ChangeResult(
    ndviChange: Array[Float],
    nbrChange: Array[Float],
    disturbanceScore: Array[Float],
    changeType: Array[Int],
    width: Int,
    height: Int
):
  def pixelCount: Int = width * height
  
  /** Count pixels by change type */
  def changeTypeCounts: Map[String, Int] =
    val types = Map(
      0 -> "No Change",
      1 -> "Fire",
      2 -> "Logging",
      3 -> "Regrowth",
      4 -> "Other Disturbance"
    )
    
    val counts = changeType.groupBy(identity).view.mapValues(_.length).toMap
    types.map { case (code, name) =>
      name -> counts.getOrElse(code, 0)
    }
  
  /** Statistics for disturbance score */
  def disturbanceStats: (Float, Float, Float) =
    val filtered = disturbanceScore.filter(_ > 0.0f)
    if filtered.isEmpty then (0.0f, 0.0f, 0.0f)
    else (filtered.min, filtered.max, filtered.sum / filtered.length)

