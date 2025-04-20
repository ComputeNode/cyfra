package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.FunctionName
import io.computenode.cyfra.dsl.Algebra.{/, FromExpr, vec3}
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Value.*
import izumi.reflect.Tag

import scala.annotation.targetName

sealed class FunctionName

object Functions:

  case object Sin extends FunctionName
  def sin(v: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Sin, List(v)))

  case object Cos extends FunctionName
  def cos(v: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Cos, List(v)))
  def cos[V <: Vec[Float32] : Tag : FromExpr](v: V)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Cos, List(v)))
  
  case object Tan extends FunctionName
  def tan(v: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Tan, List(v)))
  
  case object Acos extends FunctionName
  def acos(v: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Acos, List(v)))
  
  case object Asin extends FunctionName
  def asin(v: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Asin, List(v)))
  
  case object Atan extends FunctionName
  def atan(v: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Atan, List(v)))
  
  case object Atan2 extends FunctionName
  def atan2(y: Float32, x: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Atan2, List(y, x)))

  case object Len2 extends FunctionName
  def length[T <: Scalar : Tag](v: Vec2[T])(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Len2, List(v)))

  case object Len3 extends FunctionName
  def length[T <: Scalar : Tag](v: Vec3[T])(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Len3, List(v)))

  case object Pow extends FunctionName
  def pow(v: Float32, p: Float32)(using sourcecode.Name): Float32 = 
    Float32(ExtFunctionCall(Pow, List(v, p)))
  def pow[V <: Vec[_] : Tag : FromExpr](v: V, p: V)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Pow, List(v, p)))
  
  case object Smoothstep extends FunctionName
  def smoothstep(edge0: Float32, edge1: Float32, x: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Smoothstep, List(edge0, edge1, x)))
  
  case object Sqrt extends FunctionName
  def sqrt(v: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Sqrt, List(v)))
  
  case object Cross extends FunctionName
  def cross[T <: Scalar : Tag](v1: Vec3[T], v2: Vec3[T])(using sourcecode.Name): Vec3[T] = Vec3(ExtFunctionCall(Cross, List(v1, v2)))

  case object Clamp extends FunctionName
  def clamp(f: Float32, from: Float32, to: Float32)(using sourcecode.Name): Float32 =
    Float32(ExtFunctionCall(Clamp, List(f, from, to)))

  case object Exp extends FunctionName
  def exp(f: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Exp, List(f)))
  def exp[V <: Vec[Float32] : Tag : FromExpr](v: V)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Exp, List(v)))

  case object Max extends FunctionName
  def max(f1: Float32, f2: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Max, List(f1, f2)))
  def max(f1: Float32, f2: Float32, fx: Float32*)(using sourcecode.Name): Float32 = fx.foldLeft(max(f1, f2))((a, b) => max(a, b))
  def max[V <: Vec[Float32] : Tag : FromExpr](v1: V, v2: V)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Max, List(v1, v2)))
  def max[V <: Vec[Float32] : Tag : FromExpr](v1: V, v2: V, vx: V*)(using sourcecode.Name): V =
    vx.foldLeft(max(v1, v2))((a, b) => max(a, b))

  case object Min extends FunctionName
  def min(f1: Float32, f2: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Min, List(f1, f2)))
  def min(f1: Float32, f2: Float32, fx: Float32*)(using sourcecode.Name): Float32 = fx.foldLeft(min(f1, f2))((a, b) => min(a, b))
  def min[V <: Vec[Float32] : Tag : FromExpr](v1: V, v2: V)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Min, List(v1, v2)))
  def min[V <: Vec[Float32] : Tag : FromExpr](v1: V, v2: V, vx: V*)(using sourcecode.Name): V =
    vx.foldLeft(min(v1, v2))((a, b) => min(a, b))


  // todo add F/U/S to all functions that need it
  case object Abs extends FunctionName
  def abs(f: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Abs, List(f)))
  def abs[V <: Vec[Float32] : Tag : FromExpr](v: V)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Abs, List(v)))

  case object Mix extends FunctionName
  def mix[V <: Vec[Float32] : Tag : FromExpr](a: V, b: V, t: V)(using sourcecode.Name) =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Mix, List(a, b, t)))
  def mix(a: Float32, b: Float32, t: Float32)(using sourcecode.Name) = Float32(ExtFunctionCall(Mix, List(a, b, t)))
  def mix[V <: Vec[Float32] : Tag : FromExpr](a: V, b: V, t: Float32)(using sourcecode.Name) =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Mix, List(a, b, vec3(t))))
    
  case object Reflect extends FunctionName
  def reflect[I <: Vec[Float32] : Tag : FromExpr, N <: Vec[Float32] : Tag : FromExpr](I: I, N: N)(using sourcecode.Name): I =
    summon[FromExpr[I]].fromExpr(ExtFunctionCall(Reflect, List(I, N)))

  case object Refract extends FunctionName
  def refract[V <: Vec[Float32] : Tag : FromExpr](I: V, N: V, eta: Float32)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Refract, List(I, N, eta)))

  case object Normalize extends FunctionName
  def normalize[V <: Vec[Float32] : Tag : FromExpr](v: V)(using sourcecode.Name): V =
    summon[FromExpr[V]].fromExpr(ExtFunctionCall(Normalize, List(v)))

  case object Log extends FunctionName
  def logn(f: Float32)(using sourcecode.Name): Float32 = Float32(ExtFunctionCall(Log, List(f)))
  def log(f: Float32, base: Float32)(using sourcecode.Name): Float32 = logn(f) / logn(base)