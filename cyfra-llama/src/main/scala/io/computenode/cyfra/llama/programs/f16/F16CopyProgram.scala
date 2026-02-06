package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 buffer copy. Used to save state for residual connections.
  * 
  * Optimized pattern matching llama.cpp's contig_cpy:
  * - 128 threads per workgroup
  * - 4 elements per thread (512 elements per workgroup)
  * - Loop unrolling for better GPU performance
  * - No bounds check (assumes size divisible by 512)
  */
object F16CopyProgram:
  private val NumThreads = 128
  private val NumIter = 4
  private val ElementsPerWorkgroup = NumThreads * NumIter // 512
  
  case class Sizes(size: Int)
  
  case class ProgramLayout(
    input: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val numIterVal: Int32 = NumIter
    val numThreadsVal: Int32 = NumThreads
    val elementsPerWgVal: Int32 = ElementsPerWorkgroup
    
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float16](s.size),
        output = GBuffer[Float16](s.size),
      ),
      dispatch = (_, s) => StaticDispatch(((s.size + ElementsPerWorkgroup - 1) / ElementsPerWorkgroup, 1, 1)),
      workgroupSize = (NumThreads, 1, 1),
    ): layout =>
      val localId = GIO.localInvocationIndex
      val workgroupId = GIO.workgroupId.x
      
      // Base index for this workgroup (each workgroup processes 512 elements)
      val wgBase = workgroupId * elementsPerWgVal
      
      // Unrolled loop with 4 iterations per thread
      GIO.repeatUnroll(numIterVal): i =>
        val idx = wgBase + localId + i * numThreadsVal
        val value = GIO.read[Float16](layout.input, idx)
        GIO.write[Float16](layout.output, idx, value)
