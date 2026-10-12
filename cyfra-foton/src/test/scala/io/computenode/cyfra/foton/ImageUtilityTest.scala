package io.computenode.cyfra.foton

import java.nio.file.Files

class ImageUtilityTest extends munit.FunSuite:
  test("renderToImage creates missing parent directories"):
    val target = Files.createTempDirectory("cyfra-image").resolve("missing/dir/out.png")
    ImageUtility.renderToImage(Array.fill(4)((1f, 0f, 0f, 1f)), 2, target)
    assert(Files.isRegularFile(target))
