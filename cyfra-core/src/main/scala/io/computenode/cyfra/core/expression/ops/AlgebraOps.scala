package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

import scala.annotation.targetName

given [T <: NumericalType: Value]: NumericalOps[T] with {}
given [T <: NumericalType: Value]: NumericalOps[Vec2[T]] with {}
given [T <: NumericalType: Value]: NumericalOps[Vec3[T]] with {}
given [T <: NumericalType: Value]: NumericalOps[Vec4[T]] with {}

trait NumericalOps[T]

extension [T: {NumericalOps, Value}](self: T)
  @targetName("add")
  def +(that: T): T = self.map(that)(BuildInFunction.Add)
  @targetName("sub")
  def -(that: T): T = self.map(that)(BuildInFunction.Sub)
  @targetName("mul")
  def *(that: T): T = self.map(that)(BuildInFunction.Mul)
  @targetName("div")
  def /(that: T): T = self.map(that)(BuildInFunction.Div)
  @targetName("mod")
  def %(that: T): T = self.map(that)(BuildInFunction.Mod)

// Vector * Scalar
extension [T <: FloatType: Value, V <: Vec[T]: Value](vec: V)
  @targetName("vectorTimesScalar")
  def *(scalar: T): V = vec.map(scalar)(BuildInFunction.VectorTimesScalar)

extension [T <: FloatType: Value, V <: Vec[T]: Value](scalar: T)
  @targetName("scalarTimesVector")
  def *(vec: V): V = vec.map(scalar)(BuildInFunction.VectorTimesScalar)

// Matrix * Scalar
extension [T <: FloatType: Value, M <: Mat[T]: Value](mat: M)
  @targetName("matrixTimesScalar")
  def *(scalar: T): M = mat.map(scalar)(BuildInFunction.MatrixTimesScalar)

extension [T <: FloatType: Value, M <: Mat[T]: Value](scalar: T)
  @targetName("scalarTimesMatrix")
  def *(mat: M): M = mat.map(scalar)(BuildInFunction.MatrixTimesScalar)

// Dot product: Vec * Vec -> Scalar
extension [T <: FloatType: Value, V <: Vec[T]: Value](v1: V)
  @targetName("dotProduct")
  infix def dot(v2: V): T = v1.map[V, T](v2)(BuildInFunction.Dot)

// Vector * Matrix/Vector operations
extension [T <: FloatType: Value](
  vec: Vec2[T]
)(using Value[Mat2x2[T]], Value[Vec2[T]], Value[Mat2x3[T]], Value[Vec3[T]], Value[Mat2x4[T]], Value[Vec4[T]])
  @targetName("vec2TimesMat2x2")
  def *(mat: Mat2x2[T]): Vec2[T] = vec.map[Mat2x2[T], Vec2[T]](mat)(BuildInFunction.VectorTimesMatrix)
  @targetName("vec2TimesMat2x3")
  def *(mat: Mat2x3[T]): Vec3[T] = vec.map[Mat2x3[T], Vec3[T]](mat)(BuildInFunction.VectorTimesMatrix)
  @targetName("vec2TimesMat2x4")
  def *(mat: Mat2x4[T]): Vec4[T] = vec.map[Mat2x4[T], Vec4[T]](mat)(BuildInFunction.VectorTimesMatrix)

extension [T <: FloatType: Value](
  vec: Vec3[T]
)(using Value[Mat3x2[T]], Value[Vec2[T]], Value[Mat3x3[T]], Value[Vec3[T]], Value[Mat3x4[T]], Value[Vec4[T]])
  @targetName("vec3TimesMat3x2")
  def *(mat: Mat3x2[T]): Vec2[T] = vec.map[Mat3x2[T], Vec2[T]](mat)(BuildInFunction.VectorTimesMatrix)
  @targetName("vec3TimesMat3x3")
  def *(mat: Mat3x3[T]): Vec3[T] = vec.map[Mat3x3[T], Vec3[T]](mat)(BuildInFunction.VectorTimesMatrix)
  @targetName("vec3TimesMat3x4")
  def *(mat: Mat3x4[T]): Vec4[T] = vec.map[Mat3x4[T], Vec4[T]](mat)(BuildInFunction.VectorTimesMatrix)

extension [T <: FloatType: Value](
  vec: Vec4[T]
)(using Value[Mat4x2[T]], Value[Vec2[T]], Value[Mat4x3[T]], Value[Vec3[T]], Value[Mat4x4[T]], Value[Vec4[T]])
  @targetName("vec4TimesMat4x2")
  def *(mat: Mat4x2[T]): Vec2[T] = vec.map[Mat4x2[T], Vec2[T]](mat)(BuildInFunction.VectorTimesMatrix)
  @targetName("vec4TimesMat4x3")
  def *(mat: Mat4x3[T]): Vec3[T] = vec.map[Mat4x3[T], Vec3[T]](mat)(BuildInFunction.VectorTimesMatrix)
  @targetName("vec4TimesMat4x4")
  def *(mat: Mat4x4[T]): Vec4[T] = vec.map[Mat4x4[T], Vec4[T]](mat)(BuildInFunction.VectorTimesMatrix)

// Matrix * Matrix/Vector operations
extension [T <: FloatType: Value](left: Mat2x2[T])(using Value[Mat2x2[T]], Value[Mat2x3[T]], Value[Mat2x4[T]], Value[Vec2[T]])
  @targetName("mat2x2TimesVec2")
  def *(vec: Vec2[T]): Vec2[T] = left.map[Vec2[T], Vec2[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat2x2TimesMat2x2")
  def *(right: Mat2x2[T]): Mat2x2[T] = left.map[Mat2x2[T], Mat2x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat2x2TimesMat2x3")
  def *(right: Mat2x3[T]): Mat2x3[T] = left.map[Mat2x3[T], Mat2x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat2x2TimesMat2x4")
  def *(right: Mat2x4[T]): Mat2x4[T] = left.map[Mat2x4[T], Mat2x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](
  left: Mat2x3[T]
)(using Value[Mat2x3[T]], Value[Mat3x2[T]], Value[Mat2x2[T]], Value[Mat3x3[T]], Value[Mat3x4[T]], Value[Mat2x4[T]], Value[Vec2[T]], Value[Vec3[T]])
  @targetName("mat2x3TimesVec3")
  def *(vec: Vec3[T]): Vec2[T] = left.map[Vec3[T], Vec2[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat2x3TimesMat3x2")
  def *(right: Mat3x2[T]): Mat2x2[T] = left.map[Mat3x2[T], Mat2x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat2x3TimesMat3x3")
  def *(right: Mat3x3[T]): Mat2x3[T] = left.map[Mat3x3[T], Mat2x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat2x3TimesMat3x4")
  def *(right: Mat3x4[T]): Mat2x4[T] = left.map[Mat3x4[T], Mat2x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](
  left: Mat2x4[T]
)(using Value[Mat2x4[T]], Value[Mat4x2[T]], Value[Mat2x2[T]], Value[Mat4x3[T]], Value[Mat2x3[T]], Value[Mat4x4[T]], Value[Vec2[T]], Value[Vec4[T]])
  @targetName("mat2x4TimesVec4")
  def *(vec: Vec4[T]): Vec2[T] = left.map[Vec4[T], Vec2[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat2x4TimesMat4x2")
  def *(right: Mat4x2[T]): Mat2x2[T] = left.map[Mat4x2[T], Mat2x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat2x4TimesMat4x3")
  def *(right: Mat4x3[T]): Mat2x3[T] = left.map[Mat4x3[T], Mat2x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat2x4TimesMat4x4")
  def *(right: Mat4x4[T]): Mat2x4[T] = left.map[Mat4x4[T], Mat2x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](
  left: Mat3x2[T]
)(using Value[Mat3x2[T]], Value[Mat2x2[T]], Value[Mat2x3[T]], Value[Mat3x3[T]], Value[Mat2x4[T]], Value[Mat3x4[T]], Value[Vec2[T]], Value[Vec3[T]])
  @targetName("mat3x2TimesVec2")
  def *(vec: Vec2[T]): Vec3[T] = left.map[Vec2[T], Vec3[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat3x2TimesMat2x2")
  def *(right: Mat2x2[T]): Mat3x2[T] = left.map[Mat2x2[T], Mat3x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat3x2TimesMat2x3")
  def *(right: Mat2x3[T]): Mat3x3[T] = left.map[Mat2x3[T], Mat3x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat3x2TimesMat2x4")
  def *(right: Mat2x4[T]): Mat3x4[T] = left.map[Mat2x4[T], Mat3x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](left: Mat3x3[T])(using Value[Mat3x3[T]], Value[Mat3x2[T]], Value[Mat3x4[T]], Value[Vec3[T]])
  @targetName("mat3x3TimesVec3")
  def *(vec: Vec3[T]): Vec3[T] = left.map[Vec3[T], Vec3[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat3x3TimesMat3x2")
  def *(right: Mat3x2[T]): Mat3x2[T] = left.map[Mat3x2[T], Mat3x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat3x3TimesMat3x3")
  def *(right: Mat3x3[T]): Mat3x3[T] = left.map[Mat3x3[T], Mat3x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat3x3TimesMat3x4")
  def *(right: Mat3x4[T]): Mat3x4[T] = left.map[Mat3x4[T], Mat3x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](
  left: Mat3x4[T]
)(using Value[Mat3x4[T]], Value[Mat4x2[T]], Value[Mat3x2[T]], Value[Mat4x3[T]], Value[Mat3x3[T]], Value[Mat4x4[T]], Value[Vec3[T]], Value[Vec4[T]])
  @targetName("mat3x4TimesVec4")
  def *(vec: Vec4[T]): Vec3[T] = left.map[Vec4[T], Vec3[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat3x4TimesMat4x2")
  def *(right: Mat4x2[T]): Mat3x2[T] = left.map[Mat4x2[T], Mat3x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat3x4TimesMat4x3")
  def *(right: Mat4x3[T]): Mat3x3[T] = left.map[Mat4x3[T], Mat3x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat3x4TimesMat4x4")
  def *(right: Mat4x4[T]): Mat3x4[T] = left.map[Mat4x4[T], Mat3x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](
  left: Mat4x2[T]
)(using Value[Mat4x2[T]], Value[Mat2x2[T]], Value[Mat2x3[T]], Value[Mat4x3[T]], Value[Mat2x4[T]], Value[Mat4x4[T]], Value[Vec2[T]], Value[Vec4[T]])
  @targetName("mat4x2TimesVec2")
  def *(vec: Vec2[T]): Vec4[T] = left.map[Vec2[T], Vec4[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat4x2TimesMat2x2")
  def *(right: Mat2x2[T]): Mat4x2[T] = left.map[Mat2x2[T], Mat4x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat4x2TimesMat2x3")
  def *(right: Mat2x3[T]): Mat4x3[T] = left.map[Mat2x3[T], Mat4x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat4x2TimesMat2x4")
  def *(right: Mat2x4[T]): Mat4x4[T] = left.map[Mat2x4[T], Mat4x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](
  left: Mat4x3[T]
)(using Value[Mat4x3[T]], Value[Mat3x2[T]], Value[Mat4x2[T]], Value[Mat3x3[T]], Value[Mat3x4[T]], Value[Mat4x4[T]], Value[Vec3[T]], Value[Vec4[T]])
  @targetName("mat4x3TimesVec3")
  def *(vec: Vec3[T]): Vec4[T] = left.map[Vec3[T], Vec4[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat4x3TimesMat3x2")
  def *(right: Mat3x2[T]): Mat4x2[T] = left.map[Mat3x2[T], Mat4x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat4x3TimesMat3x3")
  def *(right: Mat3x3[T]): Mat4x3[T] = left.map[Mat3x3[T], Mat4x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat4x3TimesMat3x4")
  def *(right: Mat3x4[T]): Mat4x4[T] = left.map[Mat3x4[T], Mat4x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

extension [T <: FloatType: Value](left: Mat4x4[T])(using Value[Mat4x4[T]], Value[Mat4x2[T]], Value[Mat4x3[T]], Value[Vec4[T]])
  @targetName("mat4x4TimesVec4")
  def *(vec: Vec4[T]): Vec4[T] = left.map[Vec4[T], Vec4[T]](vec)(BuildInFunction.MatrixTimesVector)
  @targetName("mat4x4TimesMat4x2")
  def *(right: Mat4x2[T]): Mat4x2[T] = left.map[Mat4x2[T], Mat4x2[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat4x4TimesMat4x3")
  def *(right: Mat4x3[T]): Mat4x3[T] = left.map[Mat4x3[T], Mat4x3[T]](right)(BuildInFunction.MatrixTimesMatrix)
  @targetName("mat4x4TimesMat4x4")
  def *(right: Mat4x4[T]): Mat4x4[T] = left.map[Mat4x4[T], Mat4x4[T]](right)(BuildInFunction.MatrixTimesMatrix)

// Outer product: Vec * Vec -> Matrix
extension [T <: FloatType: Value](v1: Vec2[T])(using Value[Vec2[T]], Value[Mat2x2[T]])
  @targetName("outerProductVec2")
  infix def outer(v2: Vec2[T]): Mat2x2[T] = v1.map[Vec2[T], Mat2x2[T]](v2)(BuildInFunction.OuterProduct)

extension [T <: FloatType: Value](v1: Vec3[T])(using Value[Vec3[T]], Value[Mat3x3[T]])
  @targetName("outerProductVec3")
  infix def outer(v2: Vec3[T]): Mat3x3[T] = v1.map[Vec3[T], Mat3x3[T]](v2)(BuildInFunction.OuterProduct)

extension [T <: FloatType: Value](v1: Vec4[T])(using Value[Vec4[T]], Value[Mat4x4[T]])
  @targetName("outerProductVec4")
  infix def outer(v2: Vec4[T]): Mat4x4[T] = v1.map[Vec4[T], Mat4x4[T]](v2)(BuildInFunction.OuterProduct)
