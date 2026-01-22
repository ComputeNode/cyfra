package io.computenode.cyfra.llama.gguf

import java.io.RandomAccessFile
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.file.Path
import scala.collection.mutable

/** GGUF (GGML Universal File) format reader.
  * 
  * GGUF is llama.cpp's model format. This reader parses the file header,
  * metadata key-value pairs, and tensor information.
  * 
  * File structure:
  *   - Magic: 4 bytes ("GGUF" = 0x46554747)
  *   - Version: uint32 (currently 3)
  *   - Tensor count: uint64
  *   - KV count: uint64
  *   - Key-value pairs (metadata)
  *   - Tensor info (name, dimensions, type, offset)
  *   - Padding to alignment (default 32 bytes)
  *   - Tensor data
  */
object GGUFReader:
  val GGUF_MAGIC: Int = 0x46554747  // "GGUF"
  val GGUF_VERSION: Int = 3
  val DEFAULT_ALIGNMENT: Int = 32

  /** Value types in GGUF metadata. */
  enum ValueType(val id: Int):
    case UINT8   extends ValueType(0)
    case INT8    extends ValueType(1)
    case UINT16  extends ValueType(2)
    case INT16   extends ValueType(3)
    case UINT32  extends ValueType(4)
    case INT32   extends ValueType(5)
    case FLOAT32 extends ValueType(6)
    case BOOL    extends ValueType(7)
    case STRING  extends ValueType(8)
    case ARRAY   extends ValueType(9)
    case UINT64  extends ValueType(10)
    case INT64   extends ValueType(11)
    case FLOAT64 extends ValueType(12)

  object ValueType:
    def fromId(id: Int): ValueType = 
      ValueType.values.find(_.id == id).getOrElse(
        throw new IllegalArgumentException(s"Unknown value type: $id")
      )

  /** Quantization types for tensors. */
  enum QuantType(val id: Int, val blockSize: Int, val bytesPerBlock: Int):
    case F32    extends QuantType(0, 1, 4)
    case F16    extends QuantType(1, 1, 2)
    case Q4_0   extends QuantType(2, 32, 18)
    case Q4_1   extends QuantType(3, 32, 20)
    case Q5_0   extends QuantType(6, 32, 22)
    case Q5_1   extends QuantType(7, 32, 24)
    case Q8_0   extends QuantType(8, 32, 34)
    case Q8_1   extends QuantType(9, 32, 36)
    case Q2_K   extends QuantType(10, 256, 84)
    case Q3_K   extends QuantType(11, 256, 110)
    case Q4_K   extends QuantType(12, 256, 144)
    case Q5_K   extends QuantType(13, 256, 176)
    case Q6_K   extends QuantType(14, 256, 210)
    case Q8_K   extends QuantType(15, 256, 292)
    case IQ2_XXS extends QuantType(16, 256, 66)
    case IQ2_XS  extends QuantType(17, 256, 74)
    case IQ3_XXS extends QuantType(18, 256, 98)
    case IQ1_S   extends QuantType(19, 256, 50)
    case IQ4_NL  extends QuantType(20, 32, 18)
    case IQ3_S   extends QuantType(21, 256, 110)
    case IQ2_S   extends QuantType(22, 256, 82)
    case IQ4_XS  extends QuantType(23, 256, 136)
    case BF16    extends QuantType(30, 1, 2)

  object QuantType:
    def fromId(id: Int): QuantType =
      QuantType.values.find(_.id == id).getOrElse(
        throw new IllegalArgumentException(s"Unknown quant type: $id")
      )

  /** Metadata value can be various types. */
  sealed trait MetaValue
  case class MetaUInt8(value: Byte) extends MetaValue
  case class MetaInt8(value: Byte) extends MetaValue
  case class MetaUInt16(value: Short) extends MetaValue
  case class MetaInt16(value: Short) extends MetaValue
  case class MetaUInt32(value: Int) extends MetaValue
  case class MetaInt32(value: Int) extends MetaValue
  case class MetaFloat32(value: Float) extends MetaValue
  case class MetaBool(value: Boolean) extends MetaValue
  case class MetaString(value: String) extends MetaValue
  case class MetaUInt64(value: Long) extends MetaValue
  case class MetaInt64(value: Long) extends MetaValue
  case class MetaFloat64(value: Double) extends MetaValue
  case class MetaArray(values: Seq[MetaValue]) extends MetaValue

  /** Tensor information from GGUF file. */
  case class TensorInfo(
    name: String,
    shape: Array[Long],
    quantType: QuantType,
    offset: Long,
  ):
    def numElements: Long = shape.product
    def numBytes: Long =
      val blocks = (numElements + quantType.blockSize - 1) / quantType.blockSize
      blocks * quantType.bytesPerBlock

  /** Parsed GGUF file. */
  case class GGUFFile(
    version: Int,
    metadata: Map[String, MetaValue],
    tensors: Seq[TensorInfo],
    dataOffset: Long,
    channel: FileChannel,
  ):
    def close(): Unit = channel.close()

    /** Get metadata value as string. */
    def getString(key: String): Option[String] = metadata.get(key).collect { case MetaString(v) => v }

    /** Get metadata value as int. */
    def getInt(key: String): Option[Int] = metadata.get(key).collect {
      case MetaUInt32(v) => v
      case MetaInt32(v) => v
      case MetaUInt8(v) => v.toInt & 0xFF
      case MetaInt8(v) => v.toInt
    }

    /** Get metadata value as long. */
    def getLong(key: String): Option[Long] = metadata.get(key).collect {
      case MetaUInt64(v) => v
      case MetaInt64(v) => v
      case MetaUInt32(v) => v.toLong & 0xFFFFFFFFL
      case MetaInt32(v) => v.toLong
    }

    /** Get metadata value as float. */
    def getFloat(key: String): Option[Float] = metadata.get(key).collect {
      case MetaFloat32(v) => v
      case MetaFloat64(v) => v.toFloat
    }
    
    /** Get metadata value as string array. */
    def getStringArray(key: String): Option[Array[String]] = metadata.get(key).collect {
      case MetaArray(vals) => vals.collect { case MetaString(s) => s }.toArray
    }
    
    /** Get metadata value as float array. */
    def getFloatArray(key: String): Option[Array[Float]] = metadata.get(key).collect {
      case MetaArray(vals) => vals.collect { case MetaFloat32(f) => f }.toArray
    }

    /** Get tensor by name. */
    def getTensor(name: String): Option[TensorInfo] = tensors.find(_.name == name)

    /** Read tensor data as float array. Only works for F32 tensors. */
    def readTensorF32(tensor: TensorInfo): Array[Float] =
      require(tensor.quantType == QuantType.F32, s"Tensor ${tensor.name} is ${tensor.quantType}, not F32")
      val buffer = ByteBuffer.allocate(tensor.numBytes.toInt).order(ByteOrder.LITTLE_ENDIAN)
      channel.read(buffer, dataOffset + tensor.offset)
      buffer.flip()
      val result = Array.ofDim[Float](tensor.numElements.toInt)
      buffer.asFloatBuffer().get(result)
      result

    /** Read tensor data as raw bytes. */
    def readTensorBytes(tensor: TensorInfo): Array[Byte] =
      val buffer = ByteBuffer.allocate(tensor.numBytes.toInt)
      channel.read(buffer, dataOffset + tensor.offset)
      buffer.flip()
      val result = Array.ofDim[Byte](tensor.numBytes.toInt)
      buffer.get(result)
      result

    /** Read tensor data directly into a ByteBuffer for GPU upload.
      * Returns little-endian ordered buffer suitable for GBuffer[UInt32].
      */
    def readTensorToBuffer(tensor: TensorInfo): ByteBuffer =
      val buffer = ByteBuffer.allocateDirect(tensor.numBytes.toInt).order(ByteOrder.LITTLE_ENDIAN)
      channel.read(buffer, dataOffset + tensor.offset)
      buffer.rewind()
      buffer

    /** Read Q4_K tensor as UInt32 array for GPU upload.
      * Q4_K: 144 bytes = 36 UInt32 per 256-element block.
      */
    def readTensorQ4KAsUInt32(tensor: TensorInfo): Array[Int] =
      require(tensor.quantType == QuantType.Q4_K, s"Tensor ${tensor.name} is ${tensor.quantType}, not Q4_K")
      val bytes = readTensorBytes(tensor)
      val buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
      val numUInt32 = tensor.numBytes.toInt / 4
      val result = Array.ofDim[Int](numUInt32)
      buf.asIntBuffer().get(result)
      result
    
    /** Read any quantized tensor as UInt32 array for GPU upload.
      * Use this for tensors that may have different quantization types.
      */
    def readTensorAsUInt32(tensor: TensorInfo): Array[Int] =
      val bytes = readTensorBytes(tensor)
      val buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
      val numUInt32 = tensor.numBytes.toInt / 4
      val result = Array.ofDim[Int](numUInt32)
      buf.asIntBuffer().get(result)
      result
    
    /** Read Q6_K tensor as UInt32 array for GPU upload.
      * Q6_K: 210 bytes per 256-element block.
      * 
      * Since 210 is not divisible by 4, we pad each block to 212 bytes (53 uint32)
      * for GPU alignment.
      */
    def readTensorQ6KAsUInt32(tensor: TensorInfo): Array[Int] =
      require(tensor.quantType == QuantType.Q6_K, s"Tensor ${tensor.name} is ${tensor.quantType}, not Q6_K")
      val bytes = readTensorBytes(tensor)
      val buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
      
      // Q6_K blocks are 210 bytes. We pack them as-is, reading at byte level on GPU.
      // Just pad the total to a multiple of 4 for uint32 alignment.
      val numUInt32 = (tensor.numBytes.toInt + 3) / 4
      val result = Array.ofDim[Int](numUInt32)
      
      // Copy bytes into uint32 array
      var i = 0
      while i < tensor.numBytes.toInt / 4 do
        result(i) = buf.getInt(i * 4)
        i += 1
      
      // Handle remaining bytes (if any)
      if tensor.numBytes.toInt % 4 != 0 then
        var lastWord = 0
        var j = 0
        while j < tensor.numBytes.toInt % 4 do
          lastWord |= (bytes(tensor.numBytes.toInt - tensor.numBytes.toInt % 4 + j) & 0xFF) << (j * 8)
          j += 1
        result(i) = lastWord
      
      result

    /** Read and dequantize tensor to Float32.
      * 
      * Supports F32, F16, Q4_K, and Q6_K quantization types.
      */
    def readTensorDequantized(tensor: TensorInfo): Array[Float] =
      val bytes = readTensorBytes(tensor)
      tensor.quantType match
        case QuantType.F32 =>
          val buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
          val result = Array.ofDim[Float](tensor.numElements.toInt)
          buf.asFloatBuffer().get(result)
          result
        case QuantType.F16 =>
          Dequantize.dequantizeF16(bytes, tensor.numElements)
        case QuantType.Q4_K =>
          Dequantize.dequantizeQ4K(bytes, tensor.numElements)
        case QuantType.Q6_K =>
          Dequantize.dequantizeQ6K(bytes, tensor.numElements)
        case other =>
          throw new UnsupportedOperationException(s"Dequantization not implemented for $other")
    
    /** Read F16 tensor as raw bytes without conversion.
      * 
      * Returns the raw F16 bytes (2 bytes per element) for direct GPU upload.
      * This avoids F32 conversion, saving 2x memory.
      */
    def readTensorF16Bytes(tensor: TensorInfo): Array[Byte] =
      require(tensor.quantType == QuantType.F16, s"Expected F16 tensor, got ${tensor.quantType}")
      readTensorBytes(tensor)

  /** Read GGUF file from path. */
  def read(path: Path): GGUFFile =
    val raf = new RandomAccessFile(path.toFile, "r")
    val channel = raf.getChannel

    // Read header
    val headerBuf = ByteBuffer.allocate(24).order(ByteOrder.LITTLE_ENDIAN)
    channel.read(headerBuf, 0)
    headerBuf.flip()

    val magic = headerBuf.getInt
    if magic != GGUF_MAGIC then
      throw new IllegalArgumentException(s"Invalid GGUF magic: ${magic.toHexString}, expected ${GGUF_MAGIC.toHexString}")

    val version = headerBuf.getInt
    if version != 2 && version != 3 then
      throw new IllegalArgumentException(s"Unsupported GGUF version: $version")

    val tensorCount = headerBuf.getLong
    val kvCount = headerBuf.getLong

    // Parse key-value pairs
    var offset = 24L
    val metadata = mutable.Map[String, MetaValue]()

    for _ <- 0L until kvCount do
      val (key, value, newOffset) = readKV(channel, offset)
      metadata(key) = value
      offset = newOffset

    // Parse tensor info
    val tensors = mutable.ArrayBuffer[TensorInfo]()
    for _ <- 0L until tensorCount do
      val (tensor, newOffset) = readTensorInfo(channel, offset)
      tensors += tensor
      offset = newOffset

    // Compute data offset with alignment
    val alignment = metadata.get("general.alignment").collect { case MetaUInt32(v) => v }.getOrElse(DEFAULT_ALIGNMENT)
    val padding = offset % alignment
    val dataOffset = if padding == 0 then offset else offset + alignment - padding

    GGUFFile(version, metadata.toMap, tensors.toSeq, dataOffset, channel)

  private def readKV(channel: FileChannel, offset: Long): (String, MetaValue, Long) =
    var pos = offset

    // Read key (string)
    val (key, keyEndPos) = readString(channel, pos)
    pos = keyEndPos

    // Read value type
    val typeBuf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    channel.read(typeBuf, pos)
    typeBuf.flip()
    val valueTypeId = typeBuf.getInt
    pos += 4

    val valueType = ValueType.fromId(valueTypeId)
    val (value, valueEndPos) = readValue(channel, pos, valueType)

    (key, value, valueEndPos)

  private def readValue(channel: FileChannel, offset: Long, valueType: ValueType): (MetaValue, Long) =
    valueType match
      case ValueType.UINT8 =>
        val buf = ByteBuffer.allocate(1)
        channel.read(buf, offset)
        (MetaUInt8(buf.get(0)), offset + 1)

      case ValueType.INT8 =>
        val buf = ByteBuffer.allocate(1)
        channel.read(buf, offset)
        (MetaInt8(buf.get(0)), offset + 1)

      case ValueType.UINT16 =>
        val buf = ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaUInt16(buf.getShort(0)), offset + 2)

      case ValueType.INT16 =>
        val buf = ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaInt16(buf.getShort(0)), offset + 2)

      case ValueType.UINT32 =>
        val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaUInt32(buf.getInt(0)), offset + 4)

      case ValueType.INT32 =>
        val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaInt32(buf.getInt(0)), offset + 4)

      case ValueType.FLOAT32 =>
        val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaFloat32(buf.getFloat(0)), offset + 4)

      case ValueType.BOOL =>
        val buf = ByteBuffer.allocate(1)
        channel.read(buf, offset)
        (MetaBool(buf.get(0) != 0), offset + 1)

      case ValueType.STRING =>
        val (str, endPos) = readString(channel, offset)
        (MetaString(str), endPos)

      case ValueType.UINT64 =>
        val buf = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaUInt64(buf.getLong(0)), offset + 8)

      case ValueType.INT64 =>
        val buf = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaInt64(buf.getLong(0)), offset + 8)

      case ValueType.FLOAT64 =>
        val buf = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(buf, offset)
        (MetaFloat64(buf.getDouble(0)), offset + 8)

      case ValueType.ARRAY =>
        var pos = offset
        // Read array element type
        val typeBuf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(typeBuf, pos)
        typeBuf.flip()
        val elemTypeId = typeBuf.getInt
        pos += 4

        // Read array length
        val lenBuf = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
        channel.read(lenBuf, pos)
        lenBuf.flip()
        val arrayLen = lenBuf.getLong
        pos += 8

        val elemType = ValueType.fromId(elemTypeId)
        val values = mutable.ArrayBuffer[MetaValue]()

        for _ <- 0L until arrayLen do
          val (value, endPos) = readValue(channel, pos, elemType)
          values += value
          pos = endPos

        (MetaArray(values.toSeq), pos)

  private def readString(channel: FileChannel, offset: Long): (String, Long) =
    // Read string length (uint64)
    val lenBuf = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
    channel.read(lenBuf, offset)
    lenBuf.flip()
    val strLen = lenBuf.getLong.toInt

    // Read string bytes
    val strBuf = ByteBuffer.allocate(strLen)
    channel.read(strBuf, offset + 8)
    strBuf.flip()
    val bytes = Array.ofDim[Byte](strLen)
    strBuf.get(bytes)

    (new String(bytes, "UTF-8"), offset + 8 + strLen)

  private def readTensorInfo(channel: FileChannel, offset: Long): (TensorInfo, Long) =
    var pos = offset

    // Read tensor name
    val (name, nameEndPos) = readString(channel, pos)
    pos = nameEndPos

    // Read number of dimensions
    val dimBuf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    channel.read(dimBuf, pos)
    dimBuf.flip()
    val nDims = dimBuf.getInt
    pos += 4

    // Read dimensions
    val shape = Array.ofDim[Long](nDims)
    val shapeBuf = ByteBuffer.allocate(8 * nDims).order(ByteOrder.LITTLE_ENDIAN)
    channel.read(shapeBuf, pos)
    shapeBuf.flip()
    for i <- 0 until nDims do
      shape(i) = shapeBuf.getLong
    pos += 8 * nDims

    // Read quant type
    val qtBuf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    channel.read(qtBuf, pos)
    qtBuf.flip()
    val quantTypeId = qtBuf.getInt
    pos += 4

    // Read tensor data offset
    val offsetBuf = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
    channel.read(offsetBuf, pos)
    offsetBuf.flip()
    val tensorOffset = offsetBuf.getLong
    pos += 8

    val quantType = QuantType.fromId(quantTypeId)
    (TensorInfo(name, shape, quantType, tensorOffset), pos)
