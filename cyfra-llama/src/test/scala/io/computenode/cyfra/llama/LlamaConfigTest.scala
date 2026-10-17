package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.model.LlamaConfig

class LlamaConfigTest extends munit.FunSuite:
  test("3B parameter estimate does not overflow before widening to Long"):
    val config = LlamaConfig(3072, 8192, 24, 8, 28, 128256, 131072)
    assertEquals(config.numParameters, 3606752256L)
