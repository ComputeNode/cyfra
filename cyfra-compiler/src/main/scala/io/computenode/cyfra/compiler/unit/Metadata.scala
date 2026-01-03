package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.FunctionIR
import io.computenode.cyfra.core.GProgram.WorkDimensions
import io.computenode.cyfra.core.binding.GBinding

case class Metadata(bindings: Seq[GBinding[?]], functions: List[FunctionIR[?]], workgroupSize: WorkDimensions)
