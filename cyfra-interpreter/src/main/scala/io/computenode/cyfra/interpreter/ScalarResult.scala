package io.computenode.cyfra.interpreter

object ScalarResult:
  type ScalarRes = Float | Int | Boolean

  extension (sr: ScalarRes)
    def neg: ScalarRes = sr match
      case f: Float   => -f
      case n: Int     => -n
      case b: Boolean => !b

    infix def unary_~ : Int = sr match
      case n: Int => ~n
      case _      => throw IllegalArgumentException("bitNeg: wrong argument type")

    infix def <<(by: ScalarRes): Int = (sr, by) match
      case (n: Int, b: Int) => n << b
      case _                => throw IllegalArgumentException("<<: incompatible argument types")

    infix def >>(by: ScalarRes): Int = (sr, by) match
      case (n: Int, b: Int) => n >> b
      case _                => throw IllegalArgumentException(">>: incompatible argument types")

    infix def &(that: ScalarRes): Int = (sr, that) match
      case (m: Int, n: Int) => m & n
      case _                => throw IllegalArgumentException("&: incompatible argument types")

    infix def |(that: ScalarRes): Int = (sr, that) match
      case (m: Int, n: Int) => m | n
      case _                => throw IllegalArgumentException("|: incompatible argument types")

    infix def ^(that: ScalarRes): Int = (sr, that) match
      case (m: Int, n: Int) => m ^ n
      case _                => throw IllegalArgumentException("^: incompatible argument types")

    infix def +(that: ScalarRes): Float | Int = (sr, that) match
      case (f: Float, t: Float) => f + t
      case (n: Int, t: Int)     => n + t
      case _                    => throw IllegalArgumentException("+: incompatible argument types")

    infix def -(that: ScalarRes): Float | Int = (sr, that) match
      case (f: Float, t: Float) => f - t
      case (n: Int, t: Int)     => n - t
      case _                    => throw IllegalArgumentException("-: incompatible argument types")

    infix def *(that: ScalarRes): Float | Int = (sr, that) match
      case (f: Float, t: Float) => f * t
      case (n: Int, t: Int)     => n * t
      case _                    => throw IllegalArgumentException("*: incompatible argument types")

    infix def /(that: ScalarRes): Float | Int = (sr, that) match
      case (f: Float, t: Float) => f / t
      case (n: Int, t: Int)     => n / t
      case _                    => throw IllegalArgumentException("/: incompatible argument types")

    infix def %(that: ScalarRes): Int = (sr, that) match
      case (n: Int, t: Int) => n % t
      case _                => throw IllegalArgumentException("%: incompatible argument types")

    infix def &&(that: ScalarRes): Boolean = (sr, that) match
      case (b: Boolean, t: Boolean) => b && t
      case _                        => throw IllegalArgumentException("&&: incompatible argument types")

    infix def ||(that: ScalarRes): Boolean = (sr, that) match
      case (b: Boolean, t: Boolean) => b || t
      case _                        => throw IllegalArgumentException("||: incompatible argument types")

    infix def >(that: ScalarRes): Boolean = (sr, that) match
      case (f: Float, t: Float) => f > t
      case (n: Int, t: Int)     => n > t
      case _                    => throw IllegalArgumentException(">: incompatible argument types")

    infix def <(that: ScalarRes): Boolean = (sr, that) match
      case (f: Float, t: Float) => f < t
      case (n: Int, t: Int)     => n < t
      case _                    => throw IllegalArgumentException("<: incompatible argument types")

    infix def >=(that: ScalarRes): Boolean = (sr, that) match
      case (f: Float, t: Float) => f >= t
      case (n: Int, t: Int)     => n >= t
      case _                    => throw IllegalArgumentException(">=: incompatible argument types")

    infix def <=(that: ScalarRes): Boolean = (sr, that) match
      case (f: Float, t: Float) => f <= t
      case (n: Int, t: Int)     => n <= t
      case _                    => throw IllegalArgumentException("<=: incompatible argument types")

    infix def ===(that: ScalarRes): Boolean = (sr, that) match
      case (f: Float, t: Float)     => Math.abs(f - t) < 0.001f
      case (n: Int, t: Int)         => n == t
      case (b: Boolean, t: Boolean) => b == t
      case _                        => throw IllegalArgumentException("eqls: incompatible argument types")
