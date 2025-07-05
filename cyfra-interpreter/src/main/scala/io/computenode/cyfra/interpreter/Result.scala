package io.computenode.cyfra.interpreter

type ScalarRes = Float | Int | Boolean
type Result = ScalarRes | Vector[ScalarRes]

object Result:
  extension (sr: ScalarRes)
    def negateSc: ScalarRes = sr match
      case f: Float   => -f
      case n: Int     => -n
      case b: Boolean => !b

    infix def +(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float)     => f + t
      case (n: Int, t: Int)         => n + t
      case (b: Boolean, t: Boolean) => ???
      case _                        => throw IllegalArgumentException("+: incompatible argument types")

    infix def -(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float)     => f - t
      case (n: Int, t: Int)         => n - t
      case (b: Boolean, t: Boolean) => ???
      case _                        => throw IllegalArgumentException("-: incompatible argument types")

    infix def *(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float)     => f * t
      case (n: Int, t: Int)         => n * t
      case (b: Boolean, t: Boolean) => ???
      case _                        => throw IllegalArgumentException("*: incompatible argument types")

    infix def &&(that: ScalarRes) = (sr, that) match
      case (b: Boolean, t: Boolean) => b && t
      case _                        => throw IllegalArgumentException("&&: incompatible argument types")
    infix def ||(that: ScalarRes) = (sr, that) match
      case (b: Boolean, t: Boolean) => b || t
      case _                        => throw IllegalArgumentException("||: incompatible argument types")

  extension (v: Vector[ScalarRes])
    def scale(s: ScalarRes) = v.map(_ * s)
    def sumRes: ScalarRes = v.head match
      case f: Float   => v.asInstanceOf[Vector[Float]].sum
      case n: Int     => v.asInstanceOf[Vector[Int]].sum
      case b: Boolean => ???
    def dot(that: Vector[ScalarRes]) = v
      .zip(that)
      .map(_ * _)
      .sumRes

  extension (r: Result)
    def negate: Result = r match
      case s: ScalarRes         => s.negateSc
      case v: Vector[ScalarRes] => v.map(_.negateSc) // this is like ScalarProd
      // how to handle nested Vectors?
