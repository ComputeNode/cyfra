package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.Compiler.Config
import io.computenode.cyfra.compiler.ir.FunctionIR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.core.GProgram.WorkDimensions
import io.computenode.cyfra.core.memory.{FocusRoot, GBinding}

case class Metadata(functions: List[FunctionIR[?]], config: Config)
