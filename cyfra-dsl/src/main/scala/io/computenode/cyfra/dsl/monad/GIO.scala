package io.computenode.cyfra.dsl.monad

import io.computenode.cyfra.utility.cats.{Free, FunctionK}
import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock, Value, Var, JumpTarget, BuildInFunction, CustomFunction, given}
import io.computenode.cyfra.core.binding.GBuffer
import io.computenode.cyfra.core.expression.Value.irs
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

type GIO[T] = Free[GOps, T]

object GIO:
  val natTransformation: FunctionK[GOps, ExpressionBlock] = new FunctionK:
    def apply[T](fa: GOps[T]): ExpressionBlock[T] =
      given Value[T] = fa.v

      fa match
        case GOps.ReadBuffer(buffer, index) =>
          val idx = index.irs
          val res = Expression.ReadBuffer(buffer, idx.result)
          ExpressionBlock(res, res :: idx.body)
        case x: GOps.WriteBuffer[a] =>
          given Value[a] = x.tv

          val GOps.WriteBuffer(buffer, index, value) = x
          val idx = index.irs
          val v = value.irs
          val res = Expression.WriteBuffer(buffer, idx.result, v.result)
          ExpressionBlock(res, res :: idx.body ++ v.body)
        case x: GOps.DeclareVariable[a] =>
          given Value[a] = x.tv

          val GOps.DeclareVariable(variable) = x
          val res = Expression.VarDeclare(variable)
          ExpressionBlock(res, List(res))
        case GOps.ReadVariable(variable) =>
          val res = Expression.VarRead(variable)
          ExpressionBlock(res, List(res))
        case x: GOps.WriteVariable[a] =>
          given Value[a] = x.tv

          val GOps.WriteVariable(variable, value) = x
          val v = value.irs
          val res = Expression.VarWrite(variable, v.result)
          ExpressionBlock(res, res :: v.body)
        case GOps.CallBuildIn0(func) =>
          val next = Expression.BuildInOperation(func, List())
          ExpressionBlock(next, next :: Nil)
        case x: GOps.CallBuildIn1[a, T] =>
          given Value[a] = x.tv

          val GOps.CallBuildIn1(func, arg) = x
          val a = arg.irs
          val next = Expression.BuildInOperation(func, List(a.result))
          ExpressionBlock(next, next :: a.body)
        case x: GOps.CallBuildIn2[a1, a2, T] =>
          given Value[a1] = x.tv1
          given Value[a2] = x.tv2

          val GOps.CallBuildIn2(func, arg1, arg2) = x
          val a1 = arg1.irs
          val a2 = arg2.irs
          val next = Expression.BuildInOperation(func, List(a1.result, a2.result))
          ExpressionBlock(next, next :: a1.body ++ a2.body)
        case x: GOps.CallBuildIn3[a1, a2, a3, T] =>
          given Value[a1] = x.tv1
          given Value[a2] = x.tv2
          given Value[a3] = x.tv3

          val GOps.CallBuildIn3(func, arg1, arg2, arg3) = x
          val a1 = arg1.irs
          val a2 = arg2.irs
          val a3 = arg3.irs
          val next = Expression.BuildInOperation(func, List(a1.result, a2.result, a3.result))
          ExpressionBlock(next, next :: a1.body ++ a2.body ++ a3.body)
        case x: GOps.CallBuildIn4[a1, a2, a3, a4, T] =>
          given Value[a1] = x.tv1
          given Value[a2] = x.tv2
          given Value[a3] = x.tv3
          given Value[a4] = x.tv4

          val GOps.CallBuildIn4(func, arg1, arg2, arg3, arg4) = x
          val a1 = arg1.irs
          val a2 = arg2.irs
          val a3 = arg3.irs
          val a4 = arg4.irs
          val next = Expression.BuildInOperation(func, List(a1.result, a2.result, a3.result, a4.result))
          ExpressionBlock(next, next :: a1.body ++ a2.body ++ a3.body ++ a4.body)
        case x: GOps.CallCustom1[a, T] =>
          given Value[a] = x.tv

          val GOps.CallCustom1(func, arg) = x
          val next = Expression.CustomCall(func, List(arg))
          ExpressionBlock(next, next :: Nil)
        case GOps.Branch(cond, ifTrue, ifFalse, break) =>
          val c = cond.irs
          val t = ifTrue.foldMap(natTransformation)
          val f = ifFalse.foldMap(natTransformation)
          val res = Expression.Branch(c.result, t, f, break)
          ExpressionBlock(res, res :: c.body)
        case GOps.Loop(mainBody, continueBody, break, continue) =>
          val mb = mainBody.foldMap(natTransformation)
          val cb = continueBody.foldMap(natTransformation)
          val res = Expression.Loop(mb, cb, break, continue)
          ExpressionBlock(res, res :: Nil)
        case x: GOps.ConditionalJump[t] =>
          given Value[t] = x.tv

          val GOps.ConditionalJump(cond, target, value) = x
          val c = cond.irs
          val v = value.irs
          val res = Expression.ConditionalJump(c.result, target, v.result)
          ExpressionBlock(res, res :: c.body ++ v.body)
        case x: GOps.Jump[t] =>
          given Value[t] = x.tv

          val GOps.Jump(target, value) = x
          val v = value.irs
          val res = Expression.Jump(target, v.result)
          ExpressionBlock(res, res :: v.body)
