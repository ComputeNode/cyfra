package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.Tag
import scala.language.experimental.namedTuples

class ValueTest extends munit.FunSuite:

  test("value derives correctly for regular tuple"):
    type RegularTuple = (Int32, Float32, UInt32)
    val v = Value[RegularTuple]
    
    assertEquals(v.composites.size, 3)
    val List(t1, t2, t3) = v.composites.map(_.tag)
    assert(t1 =:= Tag[Int32])
    assert(t2 =:= Tag[Float32])
    assert(t3 =:= Tag[UInt32])
    assert(v.baseTag.get =:= Tag[Tuple])

  test("value derives correctly for named tuple"):
    type NamedTup = (x: Int32, y: Float32, z: UInt32)
    val v = Value[NamedTup]
    
    assertEquals(v.composites.size, 3)
    
    val List(t1, t2, t3) = v.composites.map(_.tag)
    assert(t1 =:= Tag[Int32])
    assert(t2 =:= Tag[Float32])
    assert(t3 =:= Tag[UInt32])
    
    assert(v.baseTag.get =:= Tag[Tuple])

  test("named tuple and regular tuple have same value structure"):
    type RegularTuple = (Int32, Float32)
    type NamedTup = (x: Int32, y: Float32)
    
    val vRegular = Value[RegularTuple]
    val vNamed = Value[NamedTup]
    
    assertEquals(vRegular.composites.size, vNamed.composites.size)
    
    val regularTags = vRegular.composites.map(_.tag)
    val namedTags = vNamed.composites.map(_.tag)
    
    regularTags.zip(namedTags).foreach: (r, n) =>
      assert(r =:= n)

  test("named tuple can be used with const"):
    type NamedTup = (x: Int32, y: Float32)
    
    val tuple: NamedTup = (Int32(42), Float32(3.14f))
    val block = Value[NamedTup].peel(tuple)
    
    assert(block.result.isInstanceOf[Expression.Constant[?]])

  test("nested named tuples"):
    type Inner = (a: Float32, b: UInt32)
    type Outer = (x: Int32, inner: Inner, z: UInt32)
    
    val v = Value[Outer]
    
    assertEquals(v.composites.size, 3)
    val List(t1, t2, t3) = v.composites.map(_.tag)
    assert(t1 =:= Tag[Int32])
    assert(t2 =:= Tag[Inner])
    assert(t3 =:= Tag[UInt32])
    
    val innerValue = v.composites(1)
    assertEquals(innerValue.composites.size, 2)
    val List(i1, i2) = innerValue.composites.map(_.tag)
    assert(i1 =:= Tag[Float32])
    assert(i2 =:= Tag[UInt32])

  test("case class derivation works"):
    case class Point(x: Float32, y: Float32)
    
    val v = Value[Point]
    assertEquals(v.composites.size, 2)
    val List(t1, t2) = v.composites.map(_.tag)
    assert(t1 =:= Tag[Float32])
    assert(t2 =:= Tag[Float32])
