package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class ExpressionBlockTest extends munit.FunSuite:
  test("subgroup addition and scans cannot be embedded as pure expressions"):
    List(Operator.SubgroupAdd, Operator.SubgroupInclusiveAdd, Operator.SubgroupExclusiveAdd).foreach: op =>
      intercept[RuntimeException]:
        Value.map(op)[Float32, Float32](Float32(1.0f))
