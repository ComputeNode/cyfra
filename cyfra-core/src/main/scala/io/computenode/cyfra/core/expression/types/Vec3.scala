package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Expression.Operation
import io.computenode.cyfra.core.expression.ops.{Vec3Ops, VecOps}
import izumi.reflect.Tag

import scala.annotation.targetName

abstract class Vec3[T <: Scalar: Value] extends Vec[T] with Vec3Ops[T, Vec3] with VecOps[T, Vec3]:
  @targetName("vec3TimesMat3x2")
  def *(mat: Mat3x2[T])(using T <:< FloatType, Value[Vec3[T]], Value[Mat3x2[T]], Value[Vec2[T]]): Vec2[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec3[T], Mat3x2[T], Vec2[T]](this, mat)
  @targetName("vec3TimesMat3x3")
  def *(mat: Mat3x3[T])(using T <:< FloatType, Value[Vec3[T]], Value[Mat3x3[T]]): Vec3[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec3[T], Mat3x3[T], Vec3[T]](this, mat)
  @targetName("vec3TimesMat3x4")
  def *(mat: Mat3x4[T])(using T <:< FloatType, Value[Vec3[T]], Value[Mat3x4[T]], Value[Vec4[T]]): Vec4[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec3[T], Mat3x4[T], Vec4[T]](this, mat)
  @targetName("outerProductVec3")
  infix def outer(v2: Vec3[T])(using T <:< FloatType, Value[Vec3[T]], Value[Mat3x3[T]]): Mat3x3[T] =
    Value.map(Operator.OuterProduct)[Vec3[T], Vec3[T], Mat3x3[T]](this, v2)

object Vec3:
  final class Vec3Impl[T <: Scalar: Value](val block: ExpressionBlock[Vec3[T]]) extends Vec3[T] with ExpressionHolder[Vec3[T]]

  given [T <: Scalar: Value]: Value[Vec3[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Vec3[T]]): Vec3[T] = new Vec3Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Vec3[T]] = Tag[Vec3[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[Vec3].asInstanceOf[Tag[?]])

  def apply(x: Float32): Vec3[Float32] =
    val v = Value[Float32]
    val ExpressionBlock(result, body) = v.peel(x)
    val res = Expression.Combine[Vec3[Float32]](List.fill(3)(result))
    Value[Vec3[Float32]].extract(ExpressionBlock(res, res :: body))

  def apply(x: Float32, y: Float32, z: Float32): Vec3[Float32] =
    val v = Value[Float32]
    val (args, body) = List(x, y, z).map(v.peel).map(x => (x.result, x.body)).unzip
    val res = Expression.Combine[Vec3[Float32]](args)
    Value[Vec3[Float32]].extract(ExpressionBlock(res, res :: body.flatten))

  def apply(x: Float): Vec3[Float32] = const((x, x, x))
  def apply(x: Float, y: Float, z: Float): Vec3[Float32] = const((x, y, z))
  def apply(vec: (Float, Float, Float)): Vec3[Float32] = const(vec)
