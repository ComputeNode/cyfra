package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.*

import scala.annotation.targetName

object Library:

  def select[T: Value](cond: Bool, obj1: T, obj2: T): T = Value.map(Operator.Select)(cond, obj1, obj2)
  def select[V <: Vec[Bool]: Value, T <: Vec[?]: Value](cond: V, obj1: T, obj2: T): T = Value.map(Operator.Select)(cond, obj1, obj2)

  def round[T <: FloatType: Value](x: T): T = Value.map(Operator.Round)(x)
  @targetName("roundVec")
  def round[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Round)(x)

  def roundEven[T <: FloatType: Value](x: T): T = Value.map(Operator.RoundEven)(x)
  @targetName("roundEvenVec")
  def roundEven[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.RoundEven)(x)

  def trunc[T <: FloatType: Value](x: T): T = Value.map(Operator.Trunc)(x)
  @targetName("truncVec")
  def trunc[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Trunc)(x)

  def floor[T <: FloatType: Value](x: T): T = Value.map(Operator.Floor)(x)
  @targetName("floorVec")
  def floor[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Floor)(x)

  def ceil[T <: FloatType: Value](x: T): T = Value.map(Operator.Ceil)(x)
  @targetName("ceilVec")
  def ceil[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Ceil)(x)

  def fract[T <: FloatType: Value](x: T): T = Value.map(Operator.Fract)(x)
  @targetName("fractVec")
  def fract[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Fract)(x)

  def abs[T <: FloatType: Value](x: T): T = Value.map(Operator.Abs)(x)
  @targetName("absVec")
  def abs[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Abs)(x)
  def abs[T <: SignedIntType: Value](x: T): T = Value.map(Operator.Abs)(x)
  @targetName("absVecSignedInt")
  def abs[T <: SignedIntType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Abs)(x)

  def sign[T <: FloatType: Value](x: T): T = Value.map(Operator.Sign)(x)
  @targetName("signVec")
  def sign[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Sign)(x)
  def sign[T <: SignedIntType: Value](x: T): T = Value.map(Operator.Sign)(x)
  @targetName("signVecSignedInt")
  def sign[T <: SignedIntType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Sign)(x)

  def sqrt[T <: FloatType: Value](x: T): T = Value.map(Operator.Sqrt)(x)
  @targetName("sqrtVec")
  def sqrt[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Sqrt)(x)

  def inverseSqrt[T <: FloatType: Value](x: T): T = Value.map(Operator.InverseSqrt)(x)
  @targetName("inverseSqrtVec")
  def inverseSqrt[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.InverseSqrt)(x)

  def radians(x: Float16): Float16 = Value.map(Operator.Radians)(x)
  def radians(x: Float32): Float32 = Value.map(Operator.Radians)(x)
  def radians[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Radians)(x)
  @targetName("radiansVec32")
  def radians[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Radians)(x)

  def degrees(x: Float16): Float16 = Value.map(Operator.Degrees)(x)
  def degrees(x: Float32): Float32 = Value.map(Operator.Degrees)(x)
  def degrees[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Degrees)(x)
  @targetName("degreesVec32")
  def degrees[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Degrees)(x)

  def sin(x: Float16): Float16 = Value.map(Operator.Sin)(x)
  def sin(x: Float32): Float32 = Value.map(Operator.Sin)(x)
  def sin[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Sin)(x)
  @targetName("sinVec32")
  def sin[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Sin)(x)

  def cos(x: Float16): Float16 = Value.map(Operator.Cos)(x)
  def cos(x: Float32): Float32 = Value.map(Operator.Cos)(x)
  def cos[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Cos)(x)
  @targetName("cosVec32")
  def cos[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Cos)(x)

  def tan(x: Float16): Float16 = Value.map(Operator.Tan)(x)
  def tan(x: Float32): Float32 = Value.map(Operator.Tan)(x)
  def tan[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Tan)(x)
  @targetName("tanVec32")
  def tan[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Tan)(x)

  def asin(x: Float16): Float16 = Value.map(Operator.Asin)(x)
  def asin(x: Float32): Float32 = Value.map(Operator.Asin)(x)
  def asin[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Asin)(x)
  @targetName("asinVec32")
  def asin[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Asin)(x)

  def acos(x: Float16): Float16 = Value.map(Operator.Acos)(x)
  def acos(x: Float32): Float32 = Value.map(Operator.Acos)(x)
  def acos[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Acos)(x)
  @targetName("acosVec32")
  def acos[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Acos)(x)

  def atan(x: Float16): Float16 = Value.map(Operator.Atan)(x)
  def atan(x: Float32): Float32 = Value.map(Operator.Atan)(x)
  def atan[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Atan)(x)
  @targetName("atanVec32")
  def atan[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Atan)(x)

  def sinh(x: Float16): Float16 = Value.map(Operator.Sinh)(x)
  def sinh(x: Float32): Float32 = Value.map(Operator.Sinh)(x)
  def sinh[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Sinh)(x)
  @targetName("sinhVec32")
  def sinh[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Sinh)(x)

  def cosh(x: Float16): Float16 = Value.map(Operator.Cosh)(x)
  def cosh(x: Float32): Float32 = Value.map(Operator.Cosh)(x)
  def cosh[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Cosh)(x)
  @targetName("coshVec32")
  def cosh[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Cosh)(x)

  def tanh(x: Float16): Float16 = Value.map(Operator.Tanh)(x)
  def tanh(x: Float32): Float32 = Value.map(Operator.Tanh)(x)
  def tanh[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Tanh)(x)
  @targetName("tanhVec32")
  def tanh[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Tanh)(x)

  def asinh(x: Float16): Float16 = Value.map(Operator.Asinh)(x)
  def asinh(x: Float32): Float32 = Value.map(Operator.Asinh)(x)
  def asinh[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Asinh)(x)
  @targetName("asinhVec32")
  def asinh[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Asinh)(x)

  def acosh(x: Float16): Float16 = Value.map(Operator.Acosh)(x)
  def acosh(x: Float32): Float32 = Value.map(Operator.Acosh)(x)
  def acosh[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Acosh)(x)
  @targetName("acoshVec32")
  def acosh[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Acosh)(x)

  def atanh(x: Float16): Float16 = Value.map(Operator.Atanh)(x)
  def atanh(x: Float32): Float32 = Value.map(Operator.Atanh)(x)
  def atanh[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Atanh)(x)
  @targetName("atanhVec32")
  def atanh[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Atanh)(x)

  def atan2(y: Float16, x: Float16): Float16 = Value.map(Operator.Atan2)(y, x)
  def atan2(y: Float32, x: Float32): Float32 = Value.map(Operator.Atan2)(y, x)
  def atan2[V <: Vec[Float16]: Value](y: V, x: V): V = Value.map(Operator.Atan2)(y, x)
  @targetName("atan2Vec32")
  def atan2[V <: Vec[Float32]: Value](y: V, x: V): V = Value.map(Operator.Atan2)(y, x)

  def pow(x: Float16, y: Float16): Float16 = Value.map(Operator.Pow)(x, y)
  def pow(x: Float32, y: Float32): Float32 = Value.map(Operator.Pow)(x, y)
  def pow[V <: Vec[Float16]: Value](x: V, y: V): V = Value.map(Operator.Pow)(x, y)
  @targetName("powVec32")
  def pow[V <: Vec[Float32]: Value](x: V, y: V): V = Value.map(Operator.Pow)(x, y)

  def exp(x: Float16): Float16 = Value.map(Operator.Exp)(x)
  def exp(x: Float32): Float32 = Value.map(Operator.Exp)(x)
  def exp[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Exp)(x)
  @targetName("expVec32")
  def exp[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Exp)(x)

  def log(x: Float16): Float16 = Value.map(Operator.Log)(x)
  def log(x: Float32): Float32 = Value.map(Operator.Log)(x)
  def log[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Log)(x)
  @targetName("logVec32")
  def log[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Log)(x)

  def exp2(x: Float16): Float16 = Value.map(Operator.Exp2)(x)
  def exp2(x: Float32): Float32 = Value.map(Operator.Exp2)(x)
  def exp2[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Exp2)(x)
  @targetName("exp2Vec32")
  def exp2[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Exp2)(x)

  def log2(x: Float16): Float16 = Value.map(Operator.Log2)(x)
  def log2(x: Float32): Float32 = Value.map(Operator.Log2)(x)
  def log2[V <: Vec[Float16]: Value](x: V): V = Value.map(Operator.Log2)(x)
  @targetName("log2Vec32")
  def log2[V <: Vec[Float32]: Value](x: V): V = Value.map(Operator.Log2)(x)

  def min[T <: FloatType: Value](x: T, y: T): T = Value.map(Operator.Min)(x, y)
  @targetName("minVec")
  def min[T <: FloatType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.Min)(x, y)
  def min[T <: SignedIntType: Value](x: T, y: T): T = Value.map(Operator.Min)(x, y)
  @targetName("minVecSignedInt")
  def min[T <: SignedIntType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.Min)(x, y)
  def min[T <: UnsignedIntType: Value](x: T, y: T): T = Value.map(Operator.Min)(x, y)
  @targetName("minVecUnsignedInt")
  def min[T <: UnsignedIntType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.Min)(x, y)

  def max[T <: FloatType: Value](x: T, y: T): T = Value.map(Operator.Max)(x, y)
  @targetName("maxVec")
  def max[T <: FloatType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.Max)(x, y)
  def max[T <: SignedIntType: Value](x: T, y: T): T = Value.map(Operator.Max)(x, y)
  @targetName("maxVecSignedInt")
  def max[T <: SignedIntType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.Max)(x, y)
  def max[T <: UnsignedIntType: Value](x: T, y: T): T = Value.map(Operator.Max)(x, y)
  @targetName("maxVecUnsignedInt")
  def max[T <: UnsignedIntType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.Max)(x, y)

  def clamp[T <: FloatType: Value](x: T, minVal: T, maxVal: T): T = Value.map(Operator.Clamp)(x, minVal, maxVal)
  @targetName("clampVec")
  def clamp[T <: FloatType: Value, V <: Vec[T]: Value](x: V, minVal: V, maxVal: V): V = Value.map(Operator.Clamp)(x, minVal, maxVal)
  def clamp[T <: SignedIntType: Value](x: T, minVal: T, maxVal: T): T = Value.map(Operator.Clamp)(x, minVal, maxVal)
  @targetName("clampVecSignedInt")
  def clamp[T <: SignedIntType: Value, V <: Vec[T]: Value](x: V, minVal: V, maxVal: V): V = Value.map(Operator.Clamp)(x, minVal, maxVal)
  def clamp[T <: UnsignedIntType: Value](x: T, minVal: T, maxVal: T): T = Value.map(Operator.Clamp)(x, minVal, maxVal)
  @targetName("clampVecUnsignedInt")
  def clamp[T <: UnsignedIntType: Value, V <: Vec[T]: Value](x: V, minVal: V, maxVal: V): V = Value.map(Operator.Clamp)(x, minVal, maxVal)

  def nMin[T <: FloatType: Value](x: T, y: T): T = Value.map(Operator.NMin)(x, y)
  @targetName("nMinVec")
  def nMin[T <: FloatType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.NMin)(x, y)

  def nMax[T <: FloatType: Value](x: T, y: T): T = Value.map(Operator.NMax)(x, y)
  @targetName("nMaxVec")
  def nMax[T <: FloatType: Value, V <: Vec[T]: Value](x: V, y: V): V = Value.map(Operator.NMax)(x, y)

  def nClamp[T <: FloatType: Value](x: T, minVal: T, maxVal: T): T = Value.map(Operator.NClamp)(x, minVal, maxVal)
  @targetName("nClampVec")
  def nClamp[T <: FloatType: Value, V <: Vec[T]: Value](x: V, minVal: V, maxVal: V): V = Value.map(Operator.NClamp)(x, minVal, maxVal)

  def mix[T <: FloatType: Value](x: T, y: T, a: T): T = Value.map(Operator.FMix)(x, y, a)
  @targetName("mixVec")
  def mix[T <: FloatType: Value, V <: Vec[T]: Value](x: V, y: V, a: V): V = Value.map(Operator.FMix)(x, y, a)

  def step[T <: FloatType: Value](edge: T, x: T): T = Value.map(Operator.Step)(edge, x)
  @targetName("stepVec")
  def step[T <: FloatType: Value, V <: Vec[T]: Value](edge: V, x: V): V = Value.map(Operator.Step)(edge, x)

  def smoothStep[T <: FloatType: Value](edge0: T, edge1: T, x: T): T = Value.map(Operator.SmoothStep)(edge0, edge1, x)
  @targetName("smoothStepVec")
  def smoothStep[T <: FloatType: Value, V <: Vec[T]: Value](edge0: V, edge1: V, x: V): V = Value.map(Operator.SmoothStep)(edge0, edge1, x)

  def fma[T <: FloatType: Value](a: T, b: T, c: T): T = Value.map(Operator.Fma)(a, b, c)
  @targetName("fmaVec")
  def fma[T <: FloatType: Value, V <: Vec[T]: Value](a: V, b: V, c: V): V = Value.map(Operator.Fma)(a, b, c)

  def length[T <: FloatType: Value](x: T): T = Value.map(Operator.Length)(x)
  @targetName("lengthVec")
  def length[T <: FloatType: Value, V <: Vec[T]: Value](x: V): T = Value.map(Operator.Length)[V, T](x)

  def distance[T <: FloatType: Value](x: T, y: T): T = Value.map(Operator.Distance)(x, y)
  @targetName("distanceVec")
  def distance[T <: FloatType: Value, V <: Vec[T]: Value](x: V, y: V): T = Value.map(Operator.Distance)[V, V, T](x, y)

  def ldexp[T <: FloatType: Value](x: T, exp: Int32): T = Value.map(Operator.Ldexp)(x, exp)
  def ldexp[T <: FloatType: Value](x: Vec2[T], exp: Vec2[Int32])(using Value[Vec2[T]], Value[Vec2[Int32]]): Vec2[T] = Value.map(Operator.Ldexp)(x, exp)
  def ldexp[T <: FloatType: Value](x: Vec3[T], exp: Vec3[Int32])(using Value[Vec3[T]], Value[Vec3[Int32]]): Vec3[T] = Value.map(Operator.Ldexp)(x, exp)
  def ldexp[T <: FloatType: Value](x: Vec4[T], exp: Vec4[Int32])(using Value[Vec4[T]], Value[Vec4[Int32]]): Vec4[T] = Value.map(Operator.Ldexp)(x, exp)

  def cross[T <: FloatType: Value](x: Vec3[T], y: Vec3[T])(using Value[Vec3[T]]): Vec3[T] = Value.map(Operator.Cross)(x, y)

  def normalize[T <: FloatType: Value](x: T): T = Value.map(Operator.Normalize)(x)
  @targetName("normalizeVec")
  def normalize[T <: FloatType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.Normalize)(x)

  def faceForward[T <: FloatType: Value](n: T, i: T, nRef: T): T = Value.map(Operator.FaceForward)(n, i, nRef)
  @targetName("faceForwardVec")
  def faceForward[T <: FloatType: Value, V <: Vec[T]: Value](n: V, i: V, nRef: V): V = Value.map(Operator.FaceForward)(n, i, nRef)

  def reflect[T <: FloatType: Value](i: T, n: T): T = Value.map(Operator.Reflect)(i, n)
  @targetName("reflectVec")
  def reflect[T <: FloatType: Value, V <: Vec[T]: Value](i: V, n: V): V = Value.map(Operator.Reflect)(i, n)

  def refract[T <: FloatType: Value](i: T, n: T, eta: T): T = Value.map(Operator.Refract)(i, n, eta)
  @targetName("refractVec")
  def refract[T <: FloatType: Value, V <: Vec[T]: Value](i: V, n: V, eta: T): V = Value.map(Operator.Refract)[V, V, T, V](i, n, eta)

  def findLsb[T <: IntegerType: Value](x: T): T = Value.map(Operator.FindLsb)(x)
  @targetName("findLsbVec")
  def findLsb[T <: IntegerType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.FindLsb)(x)

  def findMsb[T <: SignedIntType: Value](x: T): T = Value.map(Operator.FindMsb)(x)
  @targetName("findMsbVec")
  def findMsb[T <: SignedIntType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.FindMsb)(x)
  def findMsb[T <: UnsignedIntType: Value](x: T): T = Value.map(Operator.FindMsb)(x)
  @targetName("findMsbVecUnsignedInt")
  def findMsb[T <: UnsignedIntType: Value, V <: Vec[T]: Value](x: V): V = Value.map(Operator.FindMsb)(x)

  def determinant[T <: FloatType: Value](m: Mat2x2[T])(using Value[Mat2x2[T]]): T = Value.map(Operator.Determinant)[Mat2x2[T], T](m)
  def determinant[T <: FloatType: Value](m: Mat3x3[T])(using Value[Mat3x3[T]]): T = Value.map(Operator.Determinant)[Mat3x3[T], T](m)
  def determinant[T <: FloatType: Value](m: Mat4x4[T])(using Value[Mat4x4[T]]): T = Value.map(Operator.Determinant)[Mat4x4[T], T](m)

  def matrixInverse[T <: FloatType: Value](m: Mat2x2[T])(using Value[Mat2x2[T]]): Mat2x2[T] = Value.map(Operator.MatrixInverse)(m)
  def matrixInverse[T <: FloatType: Value](m: Mat3x3[T])(using Value[Mat3x3[T]]): Mat3x3[T] = Value.map(Operator.MatrixInverse)(m)
  def matrixInverse[T <: FloatType: Value](m: Mat4x4[T])(using Value[Mat4x4[T]]): Mat4x4[T] = Value.map(Operator.MatrixInverse)(m)

  def packSnorm4x8(v: Vec4[Float32]): UInt32 = Value.map(Operator.PackSnorm4x8)[Vec4[Float32], UInt32](v)
  def packUnorm4x8(v: Vec4[Float32]): UInt32 = Value.map(Operator.PackUnorm4x8)[Vec4[Float32], UInt32](v)
  def packSnorm2x16(v: Vec2[Float32]): UInt32 = Value.map(Operator.PackSnorm2x16)[Vec2[Float32], UInt32](v)
  def packUnorm2x16(v: Vec2[Float32]): UInt32 = Value.map(Operator.PackUnorm2x16)[Vec2[Float32], UInt32](v)
  def packHalf2x16(v: Vec2[Float32]): UInt32 = Value.map(Operator.PackHalf2x16)[Vec2[Float32], UInt32](v)

  def unpackSnorm2x16(p: UInt32): Vec2[Float32] = Value.map(Operator.UnpackSnorm2x16)[UInt32, Vec2[Float32]](p)
  def unpackUnorm2x16(p: UInt32): Vec2[Float32] = Value.map(Operator.UnpackUnorm2x16)[UInt32, Vec2[Float32]](p)
  def unpackHalf2x16(v: UInt32): Vec2[Float32] = Value.map(Operator.UnpackHalf2x16)[UInt32, Vec2[Float32]](v)
  def unpackSnorm4x8(p: UInt32): Vec4[Float32] = Value.map(Operator.UnpackSnorm4x8)[UInt32, Vec4[Float32]](p)
  def unpackUnorm4x8(p: UInt32): Vec4[Float32] = Value.map(Operator.UnpackUnorm4x8)[UInt32, Vec4[Float32]](p)

  def packDouble2x32(v: Vec2[UInt32]): Float64 = Value.map(Operator.PackDouble2x32)[Vec2[UInt32], Float64](v)
  def unpackDouble2x32(v: Float64): Vec2[UInt32] = Value.map(Operator.UnpackDouble2x32)[Float64, Vec2[UInt32]](v)
