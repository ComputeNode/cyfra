import io.computenode.cyfra.dsl.archive.Value
import izumi.reflect.Tag






type Layout1 = (
  structValidityBitmap: GpBuf[Bit],
    varBinaryValidityBitmap: GpBuf[Int32],
    varBinaryOffsetsBuffer: GpBuf[Byte],
    int32ValidityBitmap: GpBuf[Bit],
    int32ValueBuffer: GpBuf[Int32],
  )

val changeLongerStringToNull: Layout1 => GIO[Unit] = {
  case (structValidityBitmap, varBinaryValidityBitmap, varBinaryOffsetsBuffer, int32ValidityBitmap, int32ValueBuffer) =>
    for {
      index <- GIO.gl_GlobalInvocationID.x
      isNotNull <- structValidityBitmap.getSafeOrDiscard(index)
      _ <- GIO.If(isNotNull) {
        for {
          length <- GIO.If(varBinaryValidityBitmap.get(index))(for {
            offset1 <- varBinaryOffsetsBuffer.get(index)
            offset2 <- varBinaryOffsetsBuffer.get(index + 1)
          } yield offset2 - offset1)(0)
          targetLength <- GIO.If(int32ValidityBitmap.get(index))(int32ValueBuffer.get(index))(Int32.Inf)
          _ <- GIO.If(length > targetLength) {
            varBinaryValidityBitmap.set(index, 0)
          }
        } yield ()
      }
    } yield ()
}

type Layout2 = (varBinaryValidityBitmap: GpBuf[Bit], varBinaryOffsetsBuffer: GpBuf[Int32], lengthsBuffer: GpBuf[Int32])

val prepareForScan: Layout2 => GIO[Unit] = { case (varBinaryValidityBitmap, varBinaryOffsetsBuffer, lengthsBuffer) =>
  for {
    index <- GIO.gl_GlobalInvocationID.x
    _ <- GIO.assertSize(lengthsBuffer, varBinaryOffsetsBuffer.length.map(identity))
    varBinaryIsPresent <- varBinaryValidityBitmap.getSafeOrDiscard(index)
    length <- GIO.If(varBinaryIsPresent) {
      for {
        offset1 <- varBinaryOffsetsBuffer.get(index)
        offset2 <- varBinaryOffsetsBuffer.get(index + 1)
      } yield offset2 - offset1
    }(0)
    _ <- lengthsBuffer.set(index, length)
  } yield ()
}


val afterScan: Layout3 => GIO[Unit] = { case (varBinaryValidityBitmap, varBinaryOffsetsBuffer, varBinaryValueBuffer, lengthsBuffer, nextBuffer) =>
  for {
    index <- GIO.gl_GlobalInvocationID.x
    _ <- GIO.assertSize(nextBuffer, varBinaryValueBuffer.length.map(identity))
    _ <- GIO.If(varBinaryValidityBitmap.getSafeOrDiscard(index)) {
      for {
        startRead <- varBinaryOffsetsBuffer.get(index)
        startWrite <- scanResult.get(index)
        endRead <- varBinaryOffsetsBuffer.get(index + 1)
        length = endRead - startRead
        _ <- GIO.Range(0, length)(i =>
          for {
            byte <- varBinaryValueBuffer.get(startRead + i)
            _ <- nextBuffer.set(startWrite + i, byte)
          } yield (),
        )
      } yield ()
    }
  } yield ()
}

val changeLongerStringToNullProgram = GProgram.compile(groupSize = (1024, 1, 1), code = changeLongerStringToNull)
val prepareForScanProgram = GProgram.compile(groupSize = (1024, 1, 1), code = prepareForScan)
val afterScanProgram = GProgram.compile(groupSize = (1024, 1, 1), code = afterScan)

val pipeline =
  GPipeline[Metadata[(Layout1, Layout2, Layout3)]]
    .invocations((metadata, shaderInfo) => (metadata.buffers("structValidityBitmap").length / shaderInfo.groupSize.x, 1, 1))
    .execute(changeLongerStringToNullProgram)
    .execute(prepareForScanProgram)
    .scan(metadata => metadata.buffers("lengthsBuffer"))
    .execute(afterScanProgram)