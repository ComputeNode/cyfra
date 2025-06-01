package io.computenode.samples.cyfra.slides2

import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.runtime.mem.{GMem, Vec4FloatMem}
import io.computenode.cyfra.utility.ImageUtility

import java.nio.file.Path

object SlidesUtility:

  def saveImage(mem: GMem[Vec4[Float32]], width: Int, height: Int, file: Path): Unit =
    ImageUtility.renderToImage(mem.asInstanceOf[Vec4FloatMem].toArray, width, height, file)