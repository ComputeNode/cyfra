import io.computenode.cyfra.dsl.Value.Int32

val inBuffer = GBuffer[Int32]()
val outBuffer = GBuffer[Int32]()

val program = GProgram.on(inBuffer, outBuffer):
  case (in, out) => for
    index <- GIO.workerIndex
    a <- in.read(index)
    _ <- out.write(index, a + 1)
    _ <- out.write(index * 2, a * 2)
  yield ()