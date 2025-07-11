package io.computenode.cyfra.interpreter

object VectorResult:
  import ScalarResult.*

  extension (v: Vector[ScalarRes])
    infix def add(that: Vector[ScalarRes]) = v.zip(that).map(_ + _)
    infix def sub(that: Vector[ScalarRes]) = v.zip(that).map(_ - _)
    infix def eql(that: Vector[ScalarRes]): Boolean = v.zip(that).forall(_ === _)
    infix def scale(s: ScalarRes) = v.map(_ * s)

    def sumRes: Float | Int = v.headOption match
      case None        => 0
      case Some(value) =>
        value match
          case f: Float   => v.asInstanceOf[Vector[Float]].sum
          case n: Int     => v.asInstanceOf[Vector[Int]].sum
          case b: Boolean => throw IllegalArgumentException("sumRes: cannot add booleans")

    infix def dot(that: Vector[ScalarRes]): Float | Int = v
      .zip(that)
      .map(_ * _)
      .sumRes
