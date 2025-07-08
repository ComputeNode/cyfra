package io.computenode.cyfra.interpreter

object Result:
  type ScalarRes = Float | Int | Boolean
  type Result = ScalarRes | Vector[ScalarRes]

  extension (sr: ScalarRes)
    def neg: ScalarRes = sr match
      case f: Float   => -f
      case n: Int     => -n
      case b: Boolean => !b

    infix def +(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f + t
      case (n: Int, t: Int)     => n + t
      case _                    => throw IllegalArgumentException("+: incompatible argument types")

    infix def -(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f - t
      case (n: Int, t: Int)     => n - t
      case _                    => throw IllegalArgumentException("-: incompatible argument types")

    infix def *(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f * t
      case (n: Int, t: Int)     => n * t
      case _                    => throw IllegalArgumentException("*: incompatible argument types")

    infix def /(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f / t
      case (n: Int, t: Int)     => n / t

    infix def %(that: ScalarRes) = (sr, that) match
      case (n: Int, t: Int) => n % t
      case _                => throw IllegalArgumentException("%: incompatible argument types")

    infix def &&(that: ScalarRes) = (sr, that) match
      case (b: Boolean, t: Boolean) => b && t
      case _                        => throw IllegalArgumentException("&&: incompatible argument types")

    infix def ||(that: ScalarRes) = (sr, that) match
      case (b: Boolean, t: Boolean) => b || t
      case _                        => throw IllegalArgumentException("||: incompatible argument types")

    infix def >(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f > t
      case (n: Int, t: Int)     => n > t
      case _                    => throw IllegalArgumentException(">: incompatible argument types")

    infix def <(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f < t
      case (n: Int, t: Int)     => n < t
      case _                    => throw IllegalArgumentException("<: incompatible argument types")

    infix def >=(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f >= t
      case (n: Int, t: Int)     => n >= t
      case _                    => throw IllegalArgumentException(">=: incompatible argument types")

    infix def <=(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float) => f <= t
      case (n: Int, t: Int)     => n <= t
      case _                    => throw IllegalArgumentException("<=: incompatible argument types")

    infix def eql(that: ScalarRes) = (sr, that) match
      case (f: Float, t: Float)     => Math.abs(f - t) < 0.001f
      case (n: Int, t: Int)         => n == t
      case (b: Boolean, t: Boolean) => b == t
      case _                        => throw IllegalArgumentException("<=: incompatible argument types")

  extension (v: Vector[ScalarRes])
    infix def add(that: Vector[ScalarRes]) = v.zip(that).map(_ + _)
    infix def sub(that: Vector[ScalarRes]) = v.zip(that).map(_ - _)
    infix def scale(s: ScalarRes) = v.map(_ * s)

    def sumRes: ScalarRes = v.headOption match
      case None        => 0
      case Some(value) =>
        value match
          case f: Float   => v.asInstanceOf[Vector[Float]].sum
          case n: Int     => v.asInstanceOf[Vector[Int]].sum
          case b: Boolean => throw IllegalArgumentException("sumRes: cannot add booleans")

    infix def dot(that: Vector[ScalarRes]) = v
      .zip(that)
      .map(_ * _)
      .sumRes

  extension (r: Result)
    def negate: Result = r match
      case s: ScalarRes         => s.neg
      case v: Vector[ScalarRes] => v.map(_.neg) // this is like ScalarProd

    infix def add(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes)                 => s + t
      case (v: Vector[ScalarRes], t: Vector[ScalarRes]) => v add t
      case _                                            => throw IllegalArgumentException("add: incompatible argument types")

    infix def sub(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes)                 => s - t
      case (v: Vector[ScalarRes], t: Vector[ScalarRes]) => v sub t
      case _                                            => throw IllegalArgumentException("sub: incompatible argument types")
