package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.ir.IR.*
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.compiler.spirv.Opcodes.*
import io.computenode.cyfra.utility.FlatList
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer

class Emitter extends CompilationModule[Compilation, ByteBuffer]:

  override def compile(input: Compilation): ByteBuffer =

    val output = input.output
    val ids = output.filter(_.isInstanceOf[RefIR[?]]).zipWithIndex.map(x => (x._1.id, ResultRef(x._2 + 1))).toMap

    val headers: List[Words] = List(
      Word(0x07230203), // Magic number
      Word(0x00010600), // SPIR-V Version: 0.1.6
      Word(0x00000000), // Generator: unknown
      Word(ids.size + 2), // id bound
      Word(0), // Reserved
    )

    def mapOperands(operands: List[Words | RefIR[?]]): List[Words] =
      operands.map:
        case w: Words    => w
        case r: RefIR[?] => ids(r.id)

    val code: List[Words] = output.map:
      case IR.SvInst(op, operands)         => Instruction(op, mapOperands(operands))
      case x @ IR.SvRef(op, tpe, operands) => Instruction(op, FlatList(tpe.map(_.id).map(ids), ids(x.id), mapOperands(operands)))
      case other                           => throw new CompilationException("Cannot emit non-SPIR-V IR: " + other)

    val bytes = (headers ++ code).flatMap(_.toWords).toArray

    BufferUtils.createByteBuffer(bytes.length).put(bytes).rewind()
