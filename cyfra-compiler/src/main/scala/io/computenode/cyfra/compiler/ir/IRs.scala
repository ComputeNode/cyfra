package io.computenode.cyfra.compiler.ir

import IR.*
import io.computenode.cyfra.compiler.ir.IRs.*
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.utility.cats.{FunctionK, ~>}

import scala.collection.mutable

case class IRs[A: Value](result: IR[A], body: List[IR[?]]):

  def filterOut(p: IR[?] => Boolean): (IRs[A], List[IR[?]]) =
    val removed = mutable.Buffer.empty[IR[?]]
    val next = flatMapReplace:
      case x if p(x) =>
        removed += x
        IRs.proxy(x)(using x.v)
      case x => IRs(x)(using x.v)
    (next, removed.toList)

  def flatMapReplace(f: IR[?] => IRs[?]): IRs[A] = flatMapReplaceImpl(f, mutable.Map.empty)

  private def flatMapReplaceImpl(f: IR[?] => IRs[?], replacements: mutable.Map[RefIR[?], RefIR[?]]): IRs[A] =
    val nextBody = body.flatMap: (x: IR[?]) =>
      val next = x match
        case b: Branch[a] =>
          given Value[a] = b.v
          val Branch(cond, ifTrue, ifFalse, t) = b
          val nextT = ifTrue.flatMapReplaceImpl(f, replacements)
          val nextF = ifFalse.flatMapReplaceImpl(f, replacements)
          Branch[a](cond, nextT, nextF, t)
        case Loop(mainBody, continueBody, b, c) =>
          val nextM = mainBody.flatMapReplaceImpl(f, replacements)
          val nextC = continueBody.flatMapReplaceImpl(f, replacements)
          Loop(nextM, nextC, b, c)
        case other => other
      val IRs(result, body) = f(next.substitute(replacements))
      result match
        case x: RefIR[?] => replacements(x) = x
      body
    val nextResult = result.substitute(replacements)
    IRs(nextResult, nextBody)

object IRs:
  def apply[A: Value](ir: IR[A]): IRs[A] = new IRs(ir, List(ir))
  def proxy[A: Value](ir: IR[A]): IRs[A] = new IRs(ir, List())
