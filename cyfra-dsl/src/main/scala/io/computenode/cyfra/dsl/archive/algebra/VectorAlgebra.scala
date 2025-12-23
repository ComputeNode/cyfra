package io.computenode.cyfra.dsl.archive.algebra

import io.computenode.cyfra.dsl.archive.Expression.*
import io.computenode.cyfra.dsl.archive.Value.*
import ScalarAlgebra.{*, given}
import io.computenode.cyfra.dsl.archive.library.Functions.{Cross, clamp}
import io.computenode.cyfra.dsl.archive.macros.Source
import izumi.reflect.Tag

import scala.annotation.targetName

object VectorAlgebra:

  trait BasicVectorAlgebra[S <: Scalar, V <: Vec[S]: {FromExpr, Tag}]
      extends VectorSummable[V]
      with VectorDiffable[V]
      with VectorDotable[S, V]
      with VectorCrossable[V]
      with VectorScalarMulable[S, V]
      with VectorNegatable[V]

  given [T <: Scalar: {FromExpr, Tag}]: BasicVectorAlgebra[T, Vec2[T]] = new BasicVectorAlgebra[T, Vec2[T]] {}
  given [T <: Scalar: {FromExpr, Tag}]: BasicVectorAlgebra[T, Vec3[T]] = new BasicVectorAlgebra[T, Vec3[T]] {}
  given [T <: Scalar: {FromExpr, Tag}]: BasicVectorAlgebra[T, Vec4[T]] = new BasicVectorAlgebra[T, Vec4[T]] {}

  trait VectorSummable[V <: Vec[?]: {FromExpr, Tag}]:
    def sum(a: V, b: V)(using Source): V = summon[FromExpr[V]].fromExpr(Sum(a, b))

  extension [V <: Vec[?]: {VectorSummable, Tag}](a: V)
    @targetName("addVector")
    inline def +(b: V)(using Source): V = summon[VectorSummable[V]].sum(a, b)

  trait VectorDiffable[V <: Vec[?]: {FromExpr, Tag}]:
    def diff(a: V, b: V)(using Source): V = summon[FromExpr[V]].fromExpr(Diff(a, b))

  extension [V <: Vec[?]: {VectorDiffable, Tag}](a: V)
    @targetName("subVector")
    inline def -(b: V)(using Source): V = summon[VectorDiffable[V]].diff(a, b)

  trait VectorDotable[S <: Scalar: {FromExpr, Tag}, V <: Vec[S]: Tag]:
    def dot(a: V, b: V)(using Source): S = summon[FromExpr[S]].fromExpr(DotProd[S, V](a, b))

  extension [S <: Scalar: Tag, V <: Vec[S]: Tag](a: V)(using VectorDotable[S, V])
    infix def dot(b: V)(using Source): S = summon[VectorDotable[S, V]].dot(a, b)

  trait VectorCrossable[V <: Vec[?]: {FromExpr, Tag}]:
    def cross(a: V, b: V)(using Source): V = summon[FromExpr[V]].fromExpr(ExtFunctionCall(Cross, List(a, b)))

  extension [V <: Vec[?]: {VectorCrossable, Tag}](a: V) infix def cross(b: V)(using Source): V = summon[VectorCrossable[V]].cross(a, b)

  trait VectorScalarMulable[S <: Scalar: Tag, V <: Vec[S]: {FromExpr, Tag}]:
    def mul(a: V, b: S)(using Source): V = summon[FromExpr[V]].fromExpr(ScalarProd[S, V](a, b))

  extension [S <: Scalar: Tag, V <: Vec[S]: Tag](a: V)(using VectorScalarMulable[S, V])
    def *(b: S)(using Source): V = summon[VectorScalarMulable[S, V]].mul(a, b)
  extension [S <: Scalar: Tag, V <: Vec[S]: Tag](s: S)(using VectorScalarMulable[S, V])
    def *(v: V)(using Source): V = summon[VectorScalarMulable[S, V]].mul(v, s)

  trait VectorNegatable[V <: Vec[?]: {FromExpr, Tag}]:
    def negate(a: V)(using Source): V = summon[FromExpr[V]].fromExpr(Negate(a))

  extension [V <: Vec[?]: {VectorNegatable, Tag}](a: V)
    @targetName("negateVector")
    def unary_-(using Source): V = summon[VectorNegatable[V]].negate(a)

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
    // Hadamard product
    inline infix def mulV(v2: Vec3[Float32]): Vec3[Float32] =
      val s = summon[ScalarMulable[Float32]]
      (s.mul(v.x, v2.x), s.mul(v.y, v2.y), s.mul(v.z, v2.z))
    inline infix def addV(v2: Vec3[Float32]): Vec3[Float32] =
      val s = summon[VectorSummable[Vec3[Float32]]]
      s.sum(v, v2)
    inline infix def divV(v2: Vec3[Float32]): Vec3[Float32] = (v.x / v2.x, v.y / v2.y, v.z / v2.z)

  inline def vclamp(v: Vec3[Float32], min: Float32, max: Float32)(using Source): Vec3[Float32] =
    (clamp(v.x, min, max), clamp(v.y, min, max), clamp(v.z, min, max))

  extension [T <: Scalar: {FromExpr, Tag}](v2: Vec2[T])
    inline def x(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v2, Int32(ConstInt32(0))))
    inline def y(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v2, Int32(ConstInt32(1))))

  extension [T <: Scalar: {FromExpr, Tag}](v3: Vec3[T])
    inline def x(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v3, Int32(ConstInt32(0))))
    inline def y(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v3, Int32(ConstInt32(1))))
    inline def z(using Source): T = summon[FromExpr[T]].fromExpr(ExtractScalar(v3, Int32(ConstInt32(2))))
    inline def r(using Source): T = x
    inline def g(using Source): T = y
    inline def b(using Source): T = z

  extension [T <: Scalar: {FromExpr, Tag}](v4: Vec4[T])
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

  given (using Source): Conversion[(Int, Int), Vec2[Int32]] = { case (x, y) =>
    Vec2(ComposeVec2(Int32(ConstInt32(x)), Int32(ConstInt32(y))))
  }

  given (using Source): Conversion[(Int32, Int32), Vec2[Int32]] = { case (x, y) =>
    Vec2(ComposeVec2(x, y))
  }

  given (using Source): Conversion[(Int32, Int32, Int32), Vec3[Int32]] = { case (x, y, z) =>
    Vec3(ComposeVec3(x, y, z))
  }

  given (using Source): Conversion[(FloatOrFloat32, FloatOrFloat32, FloatOrFloat32), Vec3[Float32]] = { case (x, y, z) =>
    Vec3(ComposeVec3(toFloat32(x), toFloat32(y), toFloat32(z)))
  }

  given (using Source): Conversion[(Int, Int, Int), Vec3[Int32]] = { case (x, y, z) =>
    Vec3(ComposeVec3(Int32(ConstInt32(x)), Int32(ConstInt32(y)), Int32(ConstInt32(z))))
  }

  given (using Source): Conversion[(Int32, Int32, Int32, Int32), Vec4[Int32]] = { case (x, y, z, w) =>
    Vec4(ComposeVec4(x, y, z, w))
  }

  given (using Source): Conversion[(FloatOrFloat32, FloatOrFloat32, FloatOrFloat32, FloatOrFloat32), Vec4[Float32]] = { case (x, y, z, w) =>
    Vec4(ComposeVec4(toFloat32(x), toFloat32(y), toFloat32(z), toFloat32(w)))
  }

  given (using Source): Conversion[(Vec3[Float32], FloatOrFloat32), Vec4[Float32]] = { case (v, w) =>
    Vec4(ComposeVec4(v.x, v.y, v.z, toFloat32(w)))
  }

  given (using Source): Conversion[(FloatOrFloat32, FloatOrFloat32), Vec2[Float32]] = { case (x, y) =>
    Vec2(ComposeVec2(toFloat32(x), toFloat32(y)))
  }
