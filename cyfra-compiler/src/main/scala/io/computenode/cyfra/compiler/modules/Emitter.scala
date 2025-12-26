package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.compiler.spirv.Opcodes.Words

class Emitter extends CompilationModule[Compilation, List[Words]]:

  override def compile(input: Compilation): List[Words] = Nil
