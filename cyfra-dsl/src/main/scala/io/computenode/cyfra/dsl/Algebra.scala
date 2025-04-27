package io.computenode.cyfra.dsl

import Algebra.FromExpr
import io.computenode.cyfra.dsl.Control.when
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

import scala.annotation.targetName
import scala.language.implicitConversions

object Algebra:

  trait FromExpr[T <: Value]:
    def fromExpr(expr: E[T])(using name: Source): T

  trait ScalarSummable[T <: Scalar: FromExpr: Tag]:
    def sum(a: T, b: T)(using name: Source): T = summon[FromExpr[T]].fromExpr(Sum(a, b))
  extension[T <: Scalar: ScalarSummable : Tag](a: T)
    @targetName("add")
    inline def +(b: T)(using Source): T = summon[ScalarSummable[T]].sum(a, b)

  trait ScalarDiffable[T <: Scalar: FromExpr: Tag]:
    def diff(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Diff(a, b))
  extension[T <: Scalar: ScalarDiffable : Tag](a: T)
    @targetName("sub")
    inline def -(b: T)(using Source): T = summon[ScalarDiffable[T]].diff(a, b)

  // T and S ??? so two
  trait ScalarMulable[T <: Scalar : FromExpr : Tag]:
    def mul(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Mul(a, b))
  extension [T <: Scalar : ScalarMulable : Tag](a: T)
    @targetName("mul")
    inline def *(b: T)(using Source): T = summon[ScalarMulable[T]].mul(a, b)

  trait ScalarDivable[T <: Scalar : FromExpr : Tag]:
    def div(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Div(a, b))
  extension [T <: Scalar : ScalarDivable : Tag](a: T)
    @targetName("div")
    inline def /(b: T)(using Source): T = summon[ScalarDivable[T]].div(a, b)
    
  trait ScalarNegatable[T <: Scalar : FromExpr : Tag]:
    def negate(a: T)(using Source): T = summon[FromExpr[T]].fromExpr(Negate(a))
  extension [T <: Scalar : ScalarNegatable : Tag](a: T)
    @targetName("negate")
    inline def unary_-(using Source): T = summon[ScalarNegatable[T]].negate(a)
    
  trait ScalarModable[T <: Scalar : FromExpr : Tag]:
    def mod(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Mod(a, b))
  extension [T <: Scalar : ScalarModable : Tag](a: T)
    inline def mod(b: T)(using Source): T = summon[ScalarModable[T]].mod(a, b)
    
  trait Comparable[T <: Scalar : FromExpr : Tag]:
    def greaterThan(a: T, b: T)(using Source): GBoolean = GBoolean(GreaterThan(a, b))
    def lessThan(a: T, b: T)(using Source): GBoolean = GBoolean(LessThan(a, b))
    def greaterThanEqual(a: T, b: T)(using Source): GBoolean = GBoolean(GreaterThanEqual(a, b))
    def lessThanEqual(a: T, b: T)(using Source): GBoolean = GBoolean(LessThanEqual(a, b))
    def equal(a: T, b: T)(using Source): GBoolean = GBoolean(Equal(a, b))
  extension [T <: Scalar : Comparable : Tag](a: T)
    inline def >(b: T)(using Source): GBoolean = summon[Comparable[T]].greaterThan(a, b)
    inline def <(b: T)(using Source): GBoolean = summon[Comparable[T]].lessThan(a, b)
    inline def >=(b: T)(using Source): GBoolean = summon[Comparable[T]].greaterThanEqual(a, b)
    inline def <=(b: T)(using Source): GBoolean = summon[Comparable[T]].lessThanEqual(a, b)
    inline def ===(b: T)(using Source): GBoolean = summon[Comparable[T]].equal(a, b)

  case class Epsilon(eps: Float)
  given Epsilon = Epsilon(0.00001f)
  
  extension (f32: Float32)
    inline def asInt(using Source): Int32 = Int32(ToInt32(f32))
    inline def =~=(other: Float32)(using epsilon: Epsilon): GBoolean =
      abs(f32 - other) < epsilon.eps
  
  extension (i32: Int32)
    inline def asFloat(using Source): Float32 = Float32(ToFloat32(i32))
    inline def unsigned(using Source): UInt32 = UInt32(ToUInt32(i32))
    
  extension (u32: UInt32)
    inline def asFloat(using Source): Float32 = Float32(ToFloat32(u32))
    inline def signed(using Source): Int32 = Int32(ToInt32(u32))
  
  trait VectorSummable[V <: Vec[_] : FromExpr : Tag]:
    def sum(a: V, b: V)(using Source): V = summon[FromExpr[V]].fromExpr(Sum(a, b))
  extension[V <: Vec[_] : VectorSummable : Tag](a: V)
    @targetName("addVector")
    inline def +(b: V)(using Source): V = summon[VectorSummable[V]].sum(a, b)

  trait VectorDiffable[V <: Vec[_] : FromExpr : Tag]:
    def diff(a: V, b: V)(using Source): V = summon[FromExpr[V]].fromExpr(Diff(a, b))
  extension[V <: Vec[_] : VectorDiffable : Tag](a: V)
    @targetName("subVector")
    inline def -(b: V)(using Source): V = summon[VectorDiffable[V]].diff(a, b)

  trait VectorDotable[S <: Scalar : FromExpr : Tag, V <: Vec[S] : Tag]:
    def dot(a: V, b: V)(using Source): S = summon[FromExpr[S]].fromExpr(DotProd[S, V](a, b))
  extension[S <: Scalar : Tag, V <: Vec[S] : Tag](a: V)(using VectorDotable[S, V])
    def dot(b: V)(using Source): S = summon[VectorDotable[S, V]].dot(a, b)

  trait VectorCrossable[V <: Vec[_] : FromExpr : Tag]:
    def cross(a: V, b: V)(using Source): V = summon[FromExpr[V]].fromExpr(ExtFunctionCall(Cross, List(a, b)))
  extension[V <: Vec[_] : VectorCrossable : Tag](a: V)
    def cross(b: V)(using Source): V = summon[VectorCrossable[V]].cross(a, b)

  trait VectorScalarMulable[S <: Scalar : Tag, V <: Vec[S] : FromExpr : Tag]:
    def mul(a: V, b: S)(using Source): V = summon[FromExpr[V]].fromExpr(ScalarProd[S, V](a, b))
  extension[S <: Scalar : Tag, V <: Vec[S] : Tag](a: V)(using VectorScalarMulable[S, V])
    def *(b: S)(using Source): V = summon[VectorScalarMulable[S, V]].mul(a, b)
  extension[S <: Scalar : Tag, V <: Vec[S] : Tag](s: S)(using VectorScalarMulable[S, V])
    def *(v: V)(using Source): V = summon[VectorScalarMulable[S, V]].mul(v, s)
    
  trait VectorNegatable[V <: Vec[_] : FromExpr : Tag]:
    def negate(a: V)(using Source): V = summon[FromExpr[V]].fromExpr(Negate(a))
  extension[V <: Vec[_] : VectorNegatable : Tag](a: V)
    @targetName("negateVector")
    def unary_-(using Source) : V = summon[VectorNegatable[V]].negate(a)
    
  trait BitwiseOperable[T <: Scalar : FromExpr : Tag]:
    def bitwiseAnd(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseAnd(a, b))
    def bitwiseOr(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseOr(a, b))
    def bitwiseXor(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseXor(a, b))
    def bitwiseNot(a: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseNot(a))
    def shiftLeft(a: T, by: UInt32)(using Source): T = summon[FromExpr[T]].fromExpr(ShiftLeft(a, by))
    def shiftRight(a: T, by: UInt32)(using Source): T = summon[FromExpr[T]].fromExpr(ShiftRight(a, by))
    
  extension[T <: Scalar : BitwiseOperable : Tag](a: T)
    inline def &(b: T)(using Source): T = summon[BitwiseOperable[T]].bitwiseAnd(a, b)
    inline def |(b: T)(using Source): T = summon[BitwiseOperable[T]].bitwiseOr(a, b)
    inline def ^(b: T)(using Source): T = summon[BitwiseOperable[T]].bitwiseXor(a, b)
    inline def unary_~(using Source) : T = summon[BitwiseOperable[T]].bitwiseNot(a)
    inline def <<(by: UInt32)(using Source): T = summon[BitwiseOperable[T]].shiftLeft(a, by)
    inline def >>(by: UInt32)(using Source): T = summon[BitwiseOperable[T]].shiftRight(a, by)

  trait BasicScalarAlgebra[T <: Scalar : FromExpr : Tag]
    extends ScalarSummable[T]
      with ScalarDiffable[T]
      with ScalarMulable[T]
      with ScalarDivable[T]
      with ScalarModable[T]
      with Comparable[T]
      with ScalarNegatable[T]

  trait BasicScalarIntAlgebra[T <: Scalar : FromExpr : Tag]
    extends BasicScalarAlgebra[T] with BitwiseOperable[T]
  
  trait BasicVectorAlgebra[S <: Scalar, V <: Vec[S] : FromExpr : Tag]
    extends VectorSummable[V]
      with VectorDiffable[V]
      with VectorDotable[S, V]
      with VectorCrossable[V]
      with VectorScalarMulable[S, V]
      with VectorNegatable[V]
  
  given BasicScalarAlgebra[Float32] = new BasicScalarAlgebra[Float32] {}
  given BasicScalarIntAlgebra[Int32] = new BasicScalarIntAlgebra[Int32] {}
  given BasicScalarIntAlgebra[UInt32] = new BasicScalarIntAlgebra[UInt32] {}

  given [T <: Scalar : FromExpr : Tag]: BasicVectorAlgebra[T, Vec2[T]] = new BasicVectorAlgebra[T, Vec2[T]] {}
  given [T <: Scalar : FromExpr : Tag]: BasicVectorAlgebra[T, Vec3[T]] = new BasicVectorAlgebra[T, Vec3[T]] {}
  given [T <: Scalar : FromExpr : Tag]: BasicVectorAlgebra[T, Vec4[T]] = new BasicVectorAlgebra[T, Vec4[T]] {}
  
  given (using Source): Conversion[Float, Float32] = f => Float32(ConstFloat32(f))
  given (using Source): Conversion[Int, Int32] = i => Int32(ConstInt32(i))
  given (using Source): Conversion[Int, UInt32] = i => UInt32(ConstUInt32(i))
  given (using Source): Conversion[Boolean, GBoolean] = b => GBoolean(ConstGB(b))
  
  type FloatOrFloat32 = Float | Float32
  
  inline def toFloat32(f: FloatOrFloat32)(using Source): Float32 = f match
    case f: Float => Float32(ConstFloat32(f))
    case f: Float32 => f
  given (using Source): Conversion[(FloatOrFloat32, FloatOrFloat32), Vec2[Float32]] = {
    case (x, y) => Vec2(ComposeVec2(toFloat32(x), toFloat32(y)))
  }
  
  given (using Source): Conversion[(Int, Int), Vec2[Int32]] = {
    case (x, y) => Vec2(ComposeVec2(Int32(ConstInt32(x)), Int32(ConstInt32(y))))
  }

  given (using Source): Conversion[(Int32, Int32), Vec2[Int32]] = {
    case (x, y) => Vec2(ComposeVec2(x, y))
  }
  
  given (using Source): Conversion[(Int32, Int32, Int32), Vec3[Int32]] = {
    case (x, y, z) => Vec3(ComposeVec3(x, y, z))
  }
  
  given (using Source): Conversion[(FloatOrFloat32, FloatOrFloat32, FloatOrFloat32), Vec3[Float32]] = {
    case (x, y, z) => Vec3(ComposeVec3(toFloat32(x), toFloat32(y), toFloat32(z)))
  }
  
  given (using Source): Conversion[(Int, Int, Int), Vec3[Int32]] = {
    case (x, y, z) => Vec3(ComposeVec3(Int32(ConstInt32(x)), Int32(ConstInt32(y)), Int32(ConstInt32(z))))
  }

  given (using Source): Conversion[(Int32, Int32, Int32, Int32), Vec4[Int32]] = {
    case (x, y, z, w) => Vec4(ComposeVec4(x, y, z, w))
  }
  given (using Source): Conversion[(FloatOrFloat32, FloatOrFloat32, FloatOrFloat32, FloatOrFloat32), Vec4[Float32]] = {
    case (x, y, z, w) => Vec4(ComposeVec4(toFloat32(x), toFloat32(y), toFloat32(z), toFloat32(w)))
  }
  given (using Source): Conversion[(Vec3[Float32], FloatOrFloat32), Vec4[Float32]] = {
    case (v, w) => Vec4(ComposeVec4(v.x, v.y, v.z, toFloat32(w)))
  }
  
  extension [T <: Scalar: FromExpr: Tag] (v2: Vec2[T])
    inline def x(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v2, Int32(ConstInt32(0))))
    inline def y(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v2, Int32(ConstInt32(1))))
    
  extension [T <: Scalar: FromExpr: Tag] (v3: Vec3[T])
    inline def x(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v3, Int32(ConstInt32(0))))
    inline def y(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v3, Int32(ConstInt32(1))))
    inline def z(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v3, Int32(ConstInt32(2))))
    inline def r(using Source): T = x
    inline def g(using Source): T = y
    inline def b(using Source): T = z

  extension [T <: Scalar: FromExpr: Tag] (v4: Vec4[T])
    inline def x(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v4, Int32(ConstInt32(0))))
    inline def y(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v4, Int32(ConstInt32(1))))
    inline def z(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v4, Int32(ConstInt32(2))))
    inline def w(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v4, Int32(ConstInt32(3))))
    inline def r(using Source): T = x
    inline def g(using Source): T = y
    inline def b(using Source): T = z
    inline def a(using Source): T = w
    inline def xyz(using Source): Vec3[T] = Vec3(ComposeVec3(x, y, z))
    inline def rgb(using Source): Vec3[T] = xyz


  extension (b: GBoolean)
    def &&(other: GBoolean)(using Source): GBoolean = GBoolean(And(b, other))
    def ||(other: GBoolean)(using Source): GBoolean = GBoolean(Or(b, other))
    def unary_!(using Source) : GBoolean = GBoolean(Not(b))


  def vec4(x: FloatOrFloat32, y: FloatOrFloat32, z: FloatOrFloat32, w: FloatOrFloat32)(using Source): Vec4[Float32] =
    Vec4(ComposeVec4(toFloat32(x), toFloat32(y), toFloat32(z), toFloat32(w)))
  def vec3(x: FloatOrFloat32, y: FloatOrFloat32, z: FloatOrFloat32)(using Source): Vec3[Float32] =
    Vec3(ComposeVec3(toFloat32(x), toFloat32(y), toFloat32(z)))
  def vec2(x: FloatOrFloat32, y: FloatOrFloat32)(using Source): Vec2[Float32] =
    Vec2(ComposeVec2(toFloat32(x), toFloat32(y)))
  
  def vec4(f: FloatOrFloat32)(using Source): Vec4[Float32] = (f, f, f, f)
  def vec3(f: FloatOrFloat32)(using Source): Vec3[Float32] = (f, f, f)
  def vec2(f: FloatOrFloat32)(using Source): Vec2[Float32] = (f, f)


  // todo below is temporary cache for functions not put as direct functions, replace below ones w/ ext functions
  extension (v: Vec3[Float32])
    inline def mulV(v2: Vec3[Float32]): Vec3[Float32] = (v.x * v2.x, v.y * v2.y, v.z * v2.z)
    inline def addV(v2: Vec3[Float32]): Vec3[Float32] = (v.x + v2.x, v.y + v2.y, v.z + v2.z)
    inline def divV(v2: Vec3[Float32]): Vec3[Float32] = (v.x / v2.x, v.y / v2.y, v.z / v2.z)


  inline def vclamp(v: Vec3[Float32], min: Float32, max: Float32)(using Source): Vec3[Float32] =
    (clamp(v.x, min, max), clamp(v.y, min, max), clamp(v.z, min, max))

  
  