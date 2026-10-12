package io.computenode.cyfra.foton

import io.computenode.cyfra.core.GCodec.given
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.library.{GSeq, pure}
import io.computenode.cyfra.runtime.VkCyfraRuntime

class GFunctionTest extends munit.FunSuite:
  given runtime: VkCyfraRuntime = VkCyfraRuntime()
  override def afterAll(): Unit = runtime.close()

  private val input = (0 until 256).toArray

  test("map element-wise"):
    val f: GFunction[Int32, Int32, Int32] = GFunction(x => x * 3 + 1)
    val result: Array[Int] = f.run(input)
    assertEquals(result.toList, input.map(_ * 3 + 1).toList)

  test("GSeq takeWhile / filter / map / fold"):
    val f: GFunction[Int32, Int32, Int32] = GFunction: x =>
      val doublings = GSeq.gen[Int32](x + 1, _ * 2).limit(20).takeWhile(_ < 100).count
      val evenSum = GSeq.gen[Int32](0, _ + 1).limit(10).filter(_ % 2 === 0).map(_ * x).fold(Int32(0), _ + _)
      doublings * 1000 + evenSum
    def doublings(x: Int) = Iterator.iterate(x + 1)(_ * 2).take(20).takeWhile(_ < 100).size
    val expected = input.map(x => doublings(x) * 1000 + (0 until 10).filter(_ % 2 == 0).map(_ * x).sum)
    val result: Array[Int] = f.run(input)
    assertEquals(result.toList, expected.toList)

  private val clampedStep = pure((x: Int32) => when(x > 100)(x - 100).otherwise(x + 1))

  test("pure function called from a loop and directly"):
    val f: GFunction[Int32, Int32, Int32] = GFunction: x =>
      GSeq.gen[Int32](x, clampedStep(_)).limit(4).fold(Int32(0), _ + _) + clampedStep(x * 2)
    def step(x: Int) = if x > 100 then x - 100 else x + 1
    val result: Array[Int] = f.run(input)
    assertEquals(result.toList, input.map(x => Iterator.iterate(x)(step).take(4).sum + step(x * 2)).toList)

  test("GSeq nested in a loop restarts on every outer iteration"):
    val f: GFunction[Int32, Int32, Int32] = GFunction: x =>
      // inner count depends on the outer element j = 1, 2, 3, so it must really rerun: 1 + 2 + 3
      GSeq.gen[Int32](1, _ + 1).limit(3).map(j => GSeq.gen[Int32](0, _ + 1).limit(4).filter(_ < j).count).fold(Int32(0), _ + _) + x * 0
    val result: Array[Int] = f.run(input)
    assertEquals(result.toList, List.fill(256)(6))
