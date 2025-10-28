package io.computenode.cyfra.satellite.spectral

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.core.*
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** GPU programs for spectral index calculation on satellite imagery.
  *
  * These programs process millions of pixels in parallel, computing spectral indices
  * that would take hours on CPU but complete in seconds on GPU.
  */
object SpectralPrograms:

  /** Layout for NDVI calculation using NIR and Red bands */
  case class NdviParams(size: Int)

  case class NdviLayout(
      nir: GBuffer[Float32],
      red: GBuffer[Float32],
      result: GBuffer[Float32]
  ) extends Layout

  /** GPU program for NDVI calculation */
  val ndviProgram = GProgram[NdviParams, NdviLayout](
    layout = params =>
      NdviLayout(
        nir = GBuffer[Float32](params.size),
        red = GBuffer[Float32](params.size),
        result = GBuffer[Float32](params.size)
      ),
    dispatch = (_, params) => GProgram.StaticDispatch(((params.size + 127) / 128, 1, 1)),
    workgroupSize = (128, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val nirValue = GIO.read(layout.nir, idx)
    val redValue = GIO.read(layout.red, idx)
    val ndviValue = SpectralIndices.ndvi(nirValue, redValue)
    GIO.write(layout.result, idx, ndviValue)

  /** Layout for EVI calculation */
  case class EviParams(size: Int)

  case class EviLayout(
      nir: GBuffer[Float32],
      red: GBuffer[Float32],
      blue: GBuffer[Float32],
      result: GBuffer[Float32]
  ) extends Layout

  val eviProgram = GProgram[EviParams, EviLayout](
    layout = params =>
      EviLayout(
        nir = GBuffer[Float32](params.size),
        red = GBuffer[Float32](params.size),
        blue = GBuffer[Float32](params.size),
        result = GBuffer[Float32](params.size)
      ),
    dispatch = (_, params) => GProgram.StaticDispatch(((params.size + 127) / 128, 1, 1)),
    workgroupSize = (128, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val nirValue = GIO.read(layout.nir, idx)
    val redValue = GIO.read(layout.red, idx)
    val blueValue = GIO.read(layout.blue, idx)
    val eviValue = SpectralIndices.evi(nirValue, redValue, blueValue)
    GIO.write(layout.result, idx, eviValue)

  /** Layout for NDWI calculation */
  case class NdwiParams(size: Int)

  case class NdwiLayout(
      green: GBuffer[Float32],
      nir: GBuffer[Float32],
      result: GBuffer[Float32]
  ) extends Layout

  val ndwiProgram = GProgram[NdwiParams, NdwiLayout](
    layout = params =>
      NdwiLayout(
        green = GBuffer[Float32](params.size),
        nir = GBuffer[Float32](params.size),
        result = GBuffer[Float32](params.size)
      ),
    dispatch = (_, params) => GProgram.StaticDispatch(((params.size + 127) / 128, 1, 1)),
    workgroupSize = (128, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val greenValue = GIO.read(layout.green, idx)
    val nirValue = GIO.read(layout.nir, idx)
    val ndwiValue = SpectralIndices.ndwi(greenValue, nirValue)
    GIO.write(layout.result, idx, ndwiValue)

  /** Layout for NBR calculation */
  case class NbrParams(size: Int)

  case class NbrLayout(
      nir: GBuffer[Float32],
      swir: GBuffer[Float32],
      result: GBuffer[Float32]
  ) extends Layout

  val nbrProgram = GProgram[NbrParams, NbrLayout](
    layout = params =>
      NbrLayout(
        nir = GBuffer[Float32](params.size),
        swir = GBuffer[Float32](params.size),
        result = GBuffer[Float32](params.size)
      ),
    dispatch = (_, params) => GProgram.StaticDispatch(((params.size + 127) / 128, 1, 1)),
    workgroupSize = (128, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val nirValue = GIO.read(layout.nir, idx)
    val swirValue = GIO.read(layout.swir, idx)
    val nbrValue = SpectralIndices.nbr(nirValue, swirValue)
    GIO.write(layout.result, idx, nbrValue)

/** High-level API for executing spectral analysis on satellite images */
class SpectralAnalyzer(using runtime: VkCyfraRuntime):
  
  // Maximum batch size: 1024*1024 pixels (power of 2)
  private val MAX_BATCH_SIZE = 1024 * 1024
  
  /** Round up to next power of 2 */
  private def nextPowerOf2(n: Int): Int =
    if n <= 0 then 1
    else
      var p = 1
      while p < n do
        p *= 2
      p
  
  /** Pad array to target size with zeros */
  private def padArray(data: Array[Float], targetSize: Int): Array[Float] =
    if data.length >= targetSize then data
    else
      val padded = Array.ofDim[Float](targetSize)
      Array.copy(data, 0, padded, 0, data.length)
      padded

  /** Compute NDVI from a satellite image */
  def computeNDVI(image: SatelliteImage): SpectralIndexResult =
    require(image.hasBand("NIR"), "Image must have NIR band")
    require(image.hasBand("Red"), "Image must have Red band")

    val pixelCount = image.pixelCount
    
    // If image fits in one batch, process directly
    if pixelCount <= MAX_BATCH_SIZE then
      processSingleBatchNDVI(image.bands("NIR"), image.bands("Red"), pixelCount, image)
    else
      processBatchedNDVI(image)
  
  /** Process a single batch of NDVI */
  private def processSingleBatchNDVI(
      nirData: Array[Float],
      redData: Array[Float],
      pixelCount: Int,
      image: SatelliteImage
  ): SpectralIndexResult =
    // Round up to nearest power of 2 for GPU efficiency
    val batchSize = nextPowerOf2(pixelCount)
    val nirPadded = padArray(nirData, batchSize)
    val redPadded = padArray(redData, batchSize)
    
    val params = SpectralPrograms.NdviParams(batchSize)

    // Create input buffers
    val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    nirBuffer.asFloatBuffer().put(nirPadded).flip()

    val redBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    redBuffer.asFloatBuffer().put(redPadded).flip()

    // Create output buffer
    val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
    val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

    // Execute on GPU
    val region = GBufferRegion
      .allocate[SpectralPrograms.NdviLayout]
      .map: region =>
        SpectralPrograms.ndviProgram.execute(params, region)

    region.runUnsafe(
      init = SpectralPrograms.NdviLayout(
        nir = GBuffer[Float32](nirBuffer),
        red = GBuffer[Float32](redBuffer),
        result = GBuffer[Float32](batchSize)
      ),
      onDone = layout => layout.result.read(resultByteBuffer)
    )

    // Copy result to array (only actual pixels, not padding)
    val result = Array.ofDim[Float](pixelCount)
    resultBuffer.get(result)

    SpectralIndexResult(result, image.width, image.height, "NDVI", image.metadata)
  
  /** Process large image in batches */
  private def processBatchedNDVI(image: SatelliteImage): SpectralIndexResult =
    val nirData = image.bands("NIR")
    val redData = image.bands("Red")
    val pixelCount = image.pixelCount
    val numBatches = (pixelCount + MAX_BATCH_SIZE - 1) / MAX_BATCH_SIZE
    
    println(s"  Processing ${pixelCount} pixels in ${numBatches} batches of ${MAX_BATCH_SIZE}")
    
    val result = Array.ofDim[Float](pixelCount)
    
    for batchIdx <- 0 until numBatches do
      val startIdx = batchIdx * MAX_BATCH_SIZE
      val endIdx = math.min(startIdx + MAX_BATCH_SIZE, pixelCount)
      val batchPixelCount = endIdx - startIdx
      val batchSize = nextPowerOf2(batchPixelCount)
      
      println(s"    Batch ${batchIdx + 1}/${numBatches}: ${batchPixelCount} pixels (padded to ${batchSize})")
      
      // Extract batch data
      val nirBatch = padArray(nirData.slice(startIdx, endIdx), batchSize)
      val redBatch = padArray(redData.slice(startIdx, endIdx), batchSize)
      
      val params = SpectralPrograms.NdviParams(batchSize)

      // Create input buffers
      val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      nirBuffer.asFloatBuffer().put(nirBatch).flip()

      val redBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      redBuffer.asFloatBuffer().put(redBatch).flip()

      // Create output buffer
      val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
      val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

      // Execute on GPU
      val region = GBufferRegion
        .allocate[SpectralPrograms.NdviLayout]
        .map: region =>
          SpectralPrograms.ndviProgram.execute(params, region)

      region.runUnsafe(
        init = SpectralPrograms.NdviLayout(
          nir = GBuffer[Float32](nirBuffer),
          red = GBuffer[Float32](redBuffer),
          result = GBuffer[Float32](batchSize)
        ),
        onDone = layout => layout.result.read(resultByteBuffer)
      )

      // Copy result (only actual pixels, not padding)
      val batchResult = Array.ofDim[Float](batchPixelCount)
      resultBuffer.get(batchResult)
      Array.copy(batchResult, 0, result, startIdx, batchPixelCount)
    
    SpectralIndexResult(result, image.width, image.height, "NDVI", image.metadata)

  /** Compute EVI from a satellite image */
  def computeEVI(image: SatelliteImage): SpectralIndexResult =
    require(image.hasBand("NIR"), "Image must have NIR band")
    require(image.hasBand("Red"), "Image must have Red band")
    require(image.hasBand("Blue"), "Image must have Blue band")

    val pixelCount = image.pixelCount
    
    // If image fits in one batch, process directly
    if pixelCount <= MAX_BATCH_SIZE then
      processSingleBatchEVI(
        image.bands("NIR"),
        image.bands("Red"),
        image.bands("Blue"),
        pixelCount,
        image
      )
    else
      processBatchedEVI(image)
  
  /** Process a single batch of EVI */
  private def processSingleBatchEVI(
      nirData: Array[Float],
      redData: Array[Float],
      blueData: Array[Float],
      pixelCount: Int,
      image: SatelliteImage
  ): SpectralIndexResult =
    val batchSize = nextPowerOf2(pixelCount)
    val nirPadded = padArray(nirData, batchSize)
    val redPadded = padArray(redData, batchSize)
    val bluePadded = padArray(blueData, batchSize)
    
    val params = SpectralPrograms.EviParams(batchSize)

    // Create input buffers
    val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    nirBuffer.asFloatBuffer().put(nirPadded).flip()

    val redBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    redBuffer.asFloatBuffer().put(redPadded).flip()

    val blueBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    blueBuffer.asFloatBuffer().put(bluePadded).flip()

    // Create output buffer
    val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
    val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

    // Execute on GPU
    val region = GBufferRegion
      .allocate[SpectralPrograms.EviLayout]
      .map: region =>
        SpectralPrograms.eviProgram.execute(params, region)

    region.runUnsafe(
      init = SpectralPrograms.EviLayout(
        nir = GBuffer[Float32](nirBuffer),
        red = GBuffer[Float32](redBuffer),
        blue = GBuffer[Float32](blueBuffer),
        result = GBuffer[Float32](batchSize)
      ),
      onDone = layout => layout.result.read(resultByteBuffer)
    )

    // Copy result to array (only actual pixels, not padding)
    val result = Array.ofDim[Float](pixelCount)
    resultBuffer.get(result)

    SpectralIndexResult(result, image.width, image.height, "EVI", image.metadata)
  
  /** Process large image in batches */
  private def processBatchedEVI(image: SatelliteImage): SpectralIndexResult =
    val nirData = image.bands("NIR")
    val redData = image.bands("Red")
    val blueData = image.bands("Blue")
    val pixelCount = image.pixelCount
    val numBatches = (pixelCount + MAX_BATCH_SIZE - 1) / MAX_BATCH_SIZE
    
    println(s"  Processing ${pixelCount} pixels in ${numBatches} batches of ${MAX_BATCH_SIZE}")
    
    val result = Array.ofDim[Float](pixelCount)
    
    for batchIdx <- 0 until numBatches do
      val startIdx = batchIdx * MAX_BATCH_SIZE
      val endIdx = math.min(startIdx + MAX_BATCH_SIZE, pixelCount)
      val batchPixelCount = endIdx - startIdx
      val batchSize = nextPowerOf2(batchPixelCount)
      
      println(s"    Batch ${batchIdx + 1}/${numBatches}: ${batchPixelCount} pixels (padded to ${batchSize})")
      
      // Extract batch data
      val nirBatch = padArray(nirData.slice(startIdx, endIdx), batchSize)
      val redBatch = padArray(redData.slice(startIdx, endIdx), batchSize)
      val blueBatch = padArray(blueData.slice(startIdx, endIdx), batchSize)
      
      val params = SpectralPrograms.EviParams(batchSize)

      // Create input buffers
      val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      nirBuffer.asFloatBuffer().put(nirBatch).flip()

      val redBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      redBuffer.asFloatBuffer().put(redBatch).flip()

      val blueBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      blueBuffer.asFloatBuffer().put(blueBatch).flip()

      // Create output buffer
      val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
      val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

      // Execute on GPU
      val region = GBufferRegion
        .allocate[SpectralPrograms.EviLayout]
        .map: region =>
          SpectralPrograms.eviProgram.execute(params, region)

      region.runUnsafe(
        init = SpectralPrograms.EviLayout(
          nir = GBuffer[Float32](nirBuffer),
          red = GBuffer[Float32](redBuffer),
          blue = GBuffer[Float32](blueBuffer),
          result = GBuffer[Float32](batchSize)
        ),
        onDone = layout => layout.result.read(resultByteBuffer)
      )

      // Copy result (only actual pixels, not padding)
      val batchResult = Array.ofDim[Float](batchPixelCount)
      resultBuffer.get(batchResult)
      Array.copy(batchResult, 0, result, startIdx, batchPixelCount)
    
    SpectralIndexResult(result, image.width, image.height, "EVI", image.metadata)

  /** Compute NDWI from a satellite image */
  def computeNDWI(image: SatelliteImage): SpectralIndexResult =
    require(image.hasBand("Green"), "Image must have Green band")
    require(image.hasBand("NIR"), "Image must have NIR band")

    val pixelCount = image.pixelCount
    
    // If image fits in one batch, process directly
    if pixelCount <= MAX_BATCH_SIZE then
      processSingleBatchNDWI(
        image.bands("Green"),
        image.bands("NIR"),
        pixelCount,
        image
      )
    else
      processBatchedNDWI(image)
  
  /** Process a single batch of NDWI */
  private def processSingleBatchNDWI(
      greenData: Array[Float],
      nirData: Array[Float],
      pixelCount: Int,
      image: SatelliteImage
  ): SpectralIndexResult =
    val batchSize = nextPowerOf2(pixelCount)
    val greenPadded = padArray(greenData, batchSize)
    val nirPadded = padArray(nirData, batchSize)
    
    val params = SpectralPrograms.NdwiParams(batchSize)

    // Create input buffers
    val greenBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    greenBuffer.asFloatBuffer().put(greenPadded).flip()

    val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    nirBuffer.asFloatBuffer().put(nirPadded).flip()

    // Create output buffer
    val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
    val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

    // Execute on GPU
    val region = GBufferRegion
      .allocate[SpectralPrograms.NdwiLayout]
      .map: region =>
        SpectralPrograms.ndwiProgram.execute(params, region)

    region.runUnsafe(
      init = SpectralPrograms.NdwiLayout(
        green = GBuffer[Float32](greenBuffer),
        nir = GBuffer[Float32](nirBuffer),
        result = GBuffer[Float32](batchSize)
      ),
      onDone = layout => layout.result.read(resultByteBuffer)
    )

    // Copy result to array (only actual pixels, not padding)
    val result = Array.ofDim[Float](pixelCount)
    resultBuffer.get(result)

    SpectralIndexResult(result, image.width, image.height, "NDWI", image.metadata)
  
  /** Process large image in batches */
  private def processBatchedNDWI(image: SatelliteImage): SpectralIndexResult =
    val greenData = image.bands("Green")
    val nirData = image.bands("NIR")
    val pixelCount = image.pixelCount
    val numBatches = (pixelCount + MAX_BATCH_SIZE - 1) / MAX_BATCH_SIZE
    
    println(s"  Processing ${pixelCount} pixels in ${numBatches} batches of ${MAX_BATCH_SIZE}")
    
    val result = Array.ofDim[Float](pixelCount)
    
    for batchIdx <- 0 until numBatches do
      val startIdx = batchIdx * MAX_BATCH_SIZE
      val endIdx = math.min(startIdx + MAX_BATCH_SIZE, pixelCount)
      val batchPixelCount = endIdx - startIdx
      val batchSize = nextPowerOf2(batchPixelCount)
      
      println(s"    Batch ${batchIdx + 1}/${numBatches}: ${batchPixelCount} pixels (padded to ${batchSize})")
      
      // Extract batch data
      val greenBatch = padArray(greenData.slice(startIdx, endIdx), batchSize)
      val nirBatch = padArray(nirData.slice(startIdx, endIdx), batchSize)
      
      val params = SpectralPrograms.NdwiParams(batchSize)

      // Create input buffers
      val greenBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      greenBuffer.asFloatBuffer().put(greenBatch).flip()

      val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      nirBuffer.asFloatBuffer().put(nirBatch).flip()

      // Create output buffer
      val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
      val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

      // Execute on GPU
      val region = GBufferRegion
        .allocate[SpectralPrograms.NdwiLayout]
        .map: region =>
          SpectralPrograms.ndwiProgram.execute(params, region)

      region.runUnsafe(
        init = SpectralPrograms.NdwiLayout(
          green = GBuffer[Float32](greenBuffer),
          nir = GBuffer[Float32](nirBuffer),
          result = GBuffer[Float32](batchSize)
        ),
        onDone = layout => layout.result.read(resultByteBuffer)
      )

      // Copy result (only actual pixels, not padding)
      val batchResult = Array.ofDim[Float](batchPixelCount)
      resultBuffer.get(batchResult)
      Array.copy(batchResult, 0, result, startIdx, batchPixelCount)
    
    SpectralIndexResult(result, image.width, image.height, "NDWI", image.metadata)

  /** Compute NBR from a satellite image */
  def computeNBR(image: SatelliteImage): SpectralIndexResult =
    require(image.hasBand("NIR"), "Image must have NIR band")
    require(image.hasBand("SWIR"), "Image must have SWIR band")

    val pixelCount = image.pixelCount
    
    // If image fits in one batch, process directly
    if pixelCount <= MAX_BATCH_SIZE then
      processSingleBatchNBR(
        image.bands("NIR"),
        image.bands("SWIR"),
        pixelCount,
        image
      )
    else
      processBatchedNBR(image)
  
  /** Process a single batch of NBR */
  private def processSingleBatchNBR(
      nirData: Array[Float],
      swirData: Array[Float],
      pixelCount: Int,
      image: SatelliteImage
  ): SpectralIndexResult =
    val batchSize = nextPowerOf2(pixelCount)
    val nirPadded = padArray(nirData, batchSize)
    val swirPadded = padArray(swirData, batchSize)
    
    val params = SpectralPrograms.NbrParams(batchSize)

    // Create input buffers
    val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    nirBuffer.asFloatBuffer().put(nirPadded).flip()

    val swirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
    swirBuffer.asFloatBuffer().put(swirPadded).flip()

    // Create output buffer
    val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
    val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

    // Execute on GPU
    val region = GBufferRegion
      .allocate[SpectralPrograms.NbrLayout]
      .map: region =>
        SpectralPrograms.nbrProgram.execute(params, region)

    region.runUnsafe(
      init = SpectralPrograms.NbrLayout(
        nir = GBuffer[Float32](nirBuffer),
        swir = GBuffer[Float32](swirBuffer),
        result = GBuffer[Float32](batchSize)
      ),
      onDone = layout => layout.result.read(resultByteBuffer)
    )

    // Copy result to array (only actual pixels, not padding)
    val result = Array.ofDim[Float](pixelCount)
    resultBuffer.get(result)

    SpectralIndexResult(result, image.width, image.height, "NBR", image.metadata)
  
  /** Process large image in batches */
  private def processBatchedNBR(image: SatelliteImage): SpectralIndexResult =
    val nirData = image.bands("NIR")
    val swirData = image.bands("SWIR")
    val pixelCount = image.pixelCount
    val numBatches = (pixelCount + MAX_BATCH_SIZE - 1) / MAX_BATCH_SIZE
    
    println(s"  Processing ${pixelCount} pixels in ${numBatches} batches of ${MAX_BATCH_SIZE}")
    
    val result = Array.ofDim[Float](pixelCount)
    
    for batchIdx <- 0 until numBatches do
      val startIdx = batchIdx * MAX_BATCH_SIZE
      val endIdx = math.min(startIdx + MAX_BATCH_SIZE, pixelCount)
      val batchPixelCount = endIdx - startIdx
      val batchSize = nextPowerOf2(batchPixelCount)
      
      println(s"    Batch ${batchIdx + 1}/${numBatches}: ${batchPixelCount} pixels (padded to ${batchSize})")
      
      // Extract batch data
      val nirBatch = padArray(nirData.slice(startIdx, endIdx), batchSize)
      val swirBatch = padArray(swirData.slice(startIdx, endIdx), batchSize)
      
      val params = SpectralPrograms.NbrParams(batchSize)

      // Create input buffers
      val nirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      nirBuffer.asFloatBuffer().put(nirBatch).flip()

      val swirBuffer = BufferUtils.createByteBuffer(batchSize * 4)
      swirBuffer.asFloatBuffer().put(swirBatch).flip()

      // Create output buffer
      val resultBuffer = BufferUtils.createFloatBuffer(batchSize)
      val resultByteBuffer = MemoryUtil.memByteBuffer(resultBuffer)

      // Execute on GPU
      val region = GBufferRegion
        .allocate[SpectralPrograms.NbrLayout]
        .map: region =>
          SpectralPrograms.nbrProgram.execute(params, region)

      region.runUnsafe(
        init = SpectralPrograms.NbrLayout(
          nir = GBuffer[Float32](nirBuffer),
          swir = GBuffer[Float32](swirBuffer),
          result = GBuffer[Float32](batchSize)
        ),
        onDone = layout => layout.result.read(resultByteBuffer)
      )

      // Copy result (only actual pixels, not padding)
      val batchResult = Array.ofDim[Float](batchPixelCount)
      resultBuffer.get(batchResult)
      Array.copy(batchResult, 0, result, startIdx, batchPixelCount)
    
    SpectralIndexResult(result, image.width, image.height, "NBR", image.metadata)

  /** Compute multiple indices - simplified version */
  def computeMultipleIndices(image: SatelliteImage): Map[String, SpectralIndexResult] =
    Map(
      "NDVI" -> computeNDVI(image),
      "EVI" -> computeEVI(image),
      "NDWI" -> computeNDWI(image),
      "NBR" -> computeNBR(image)
    )

  /** Detect changes between two images - CPU implementation for now */
  def detectChanges(
      before: SpectralIndexResult,
      after: SpectralIndexResult
  ): (SpectralIndexResult, SpectralIndexResult) =
    require(
      before.width == after.width && before.height == after.height,
      "Images must have the same dimensions"
    )

    val absChange = before.values.zip(after.values).map { case (b, a) =>
      math.abs(a - b).toFloat
    }

    val relChange = before.values.zip(after.values).map { case (b, a) =>
      if (math.abs(b) < 0.0001f) {
        if (math.abs(a) < 0.0001f) 0.0f
        else 1.0f
      } else {
        ((a - b) / math.abs(b)).toFloat
      }
    }

    (
      SpectralIndexResult(
        absChange,
        before.width,
        before.height,
        s"${before.indexName}_absolute_change",
        before.sourceMetadata
      ),
      SpectralIndexResult(
        relChange,
        before.width,
        before.height,
        s"${before.indexName}_relative_change",
        before.sourceMetadata
      )
    )
