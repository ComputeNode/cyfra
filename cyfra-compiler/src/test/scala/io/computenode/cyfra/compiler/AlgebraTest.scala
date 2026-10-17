package io.computenode.cyfra.compiler

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.modules.Algebra
import io.computenode.cyfra.compiler.unit.*
import io.computenode.cyfra.core.expression.Operator
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class AlgebraTest extends munit.FunSuite:
  test("identity conversions in directly constructed IR reuse their operand"):
    val operands = List(
      IR.Constant[Float16](1.5f),
      IR.Constant[Float32](1.5f),
      IR.Constant[Int32](1),
      IR.Constant[UInt32](1),
      IR.Constant[Vec4[Float32]]((1.0f, 2.0f, 3.0f, 4.0f)),
    )
    operands.foreach: operand =>
      val first = IR.Operation(Operator.Convert, List(operand))(using operand.v)
      val second = IR.Operation(Operator.Convert, List(first))(using operand.v)
      val input = IRs(second, List(operand, first, second))(using operand.v)
      val context = Context(ExtensionManager(), Nil, Nil, TypeManager(), ConstantsManager(), Nil)
      val (output, _) = Ctx.withCapability(context)(new Algebra().compileFunction(input))
      assert(output.result eq operand)
      assertEquals(output.body, List(operand))
