package io.computenode.cyfra.interpreter

object Result:
  export ScalarResult.*, VectorResult.*

  type Result = ScalarRes | Vector[ScalarRes]

  extension (r: Result)
    def negate: Result = r match
      case s: ScalarRes         => s.neg
      case v: Vector[ScalarRes] => v.map(_.neg) // this is like ScalarProd

    def bitNeg: Int = r match
      case sr: ScalarRes => ~sr
      case _             => throw IllegalArgumentException("bitNeg: wrong argument type")

    def shiftLeft(by: Result): Int = (r, by) match
      case (n: ScalarRes, b: ScalarRes) => n << b
      case _                            => throw IllegalArgumentException("shiftLeft: incompatible argument types")

    def shiftRight(by: Result): Int = (r, by) match
      case (n: ScalarRes, b: ScalarRes) => n >> b
      case _                            => throw IllegalArgumentException("shiftRight: incompatible argument types")

    def bitAnd(that: Result): Int = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s & t
      case _                            => throw IllegalArgumentException("bitAnd: incompatible argument types")

    def bitOr(that: Result): Int = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s | t
      case _                            => throw IllegalArgumentException("bitOr: incompatible argument types")

    def bitXor(that: Result): Int = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s ^ t
      case _                            => throw IllegalArgumentException("bitXor: incompatible argument types")

    def add(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes)                 => s + t
      case (v: Vector[ScalarRes], t: Vector[ScalarRes]) => v add t
      case _                                            => throw IllegalArgumentException("add: incompatible argument types")

    def sub(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes)                 => s - t
      case (v: Vector[ScalarRes], t: Vector[ScalarRes]) => v sub t
      case _                                            => throw IllegalArgumentException("sub: incompatible argument types")

    def mul(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s * t
      case _                            => throw IllegalArgumentException("mul: incompatible argument types")

    def div(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s / t
      case _                            => throw IllegalArgumentException("div: incompatible argument types")

    def mod(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s % t
      case _                            => throw IllegalArgumentException("mod: incompatible argument types")

    def scale(that: Result): Result = (r, that) match
      case (v: Vector[ScalarRes], t: ScalarRes) => v scale t
      case _                                    => throw IllegalArgumentException("scale: incompatible argument types")

    def dot(that: Result): Result = (r, that) match
      case (v: Vector[ScalarRes], t: Vector[ScalarRes]) => v dot t
      case _                                            => throw IllegalArgumentException("dot: incompatible argument types")

    def &&(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s && t
      case _                            => throw IllegalArgumentException("&&: incompatible argument types")

    def ||(that: Result): Result = (r, that) match
      case (s: ScalarRes, t: ScalarRes) => s || t
      case _                            => throw IllegalArgumentException("||: incompatible argument types")

    def gt(that: Result): Boolean = (r, that) match
      case (sr: ScalarRes, t: ScalarRes) => sr > t
      case _                             => throw IllegalArgumentException("gt: incompatible argument types")

    def lt(that: Result): Boolean = (r, that) match
      case (sr: ScalarRes, t: ScalarRes) => sr < t
      case _                             => throw IllegalArgumentException("lt: incompatible argument types")

    def gteq(that: Result): Boolean = (r, that) match
      case (sr: ScalarRes, t: ScalarRes) => sr >= t
      case _                             => throw IllegalArgumentException("gteq: incompatible argument types")

    def lteq(that: Result): Boolean = (r, that) match
      case (sr: ScalarRes, t: ScalarRes) => sr <= t
      case _                             => throw IllegalArgumentException("lteq: incompatible argument types")

    def eql(that: Result): Boolean = (r, that) match
      case (sr: ScalarRes, t: ScalarRes)                => sr === t
      case (v: Vector[ScalarRes], t: Vector[ScalarRes]) => v eql t
      case _                                            => throw IllegalArgumentException("eql: incompatible argument types")
