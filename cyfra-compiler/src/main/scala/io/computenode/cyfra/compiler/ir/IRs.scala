package io.computenode.cyfra.compiler.ir

import IR.*
import io.computenode.cyfra.compiler.ir.IRs.*
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.utility.cats.{FunctionK, ~>}

import scala.collection.mutable

case class IRs[A: Value](result: IR[A], body: mutable.ListBuffer[IR[?]]):

  def filterOut(p: IR[?] => Boolean): List[IR[?]] =
    val removed = mutable.Buffer.empty[IR[?]]
    flatMapReplace:
      case x if p(x) =>
        removed += x
        IRs.proxy(x)(using x.v)
      case x => IRs(x)(using x.v)
    removed.toList

  def flatMapReplace(f: IR[?] => IRs[?]): IRs[A] =
    flatMapReplaceImpl(f, mutable.Map.empty)
    this

  private def flatMapReplaceImpl(f: IR[?] => IRs[?], replacements: mutable.Map[IR[?], IR[?]]): Unit =
    body.flatMapInPlace: (x: IR[?]) =>
      x match
        case Branch(cond, ifTrue, ifFalse, _) =>
          ifTrue.flatMapReplaceImpl(f, replacements)
          ifFalse.flatMapReplaceImpl(f, replacements)
        case Loop(mainBody, continueBody, _, _) =>
          mainBody.flatMapReplace(f)
          continueBody.flatMapReplace(f)
        case _ => ()
      x.substitute(replacements)
      val IRs(result, body) = f(x)
      replacements(x) = result
      body
    ()

object IRs:
  def apply[A: Value](ir: IR[A]): IRs[A] = new IRs(ir, mutable.ListBuffer(ir))
  def apply[A: Value](ir: IR[A], body: List[IR[?]]): IRs[A] = new IRs(ir, mutable.ListBuffer.from(body))
  def proxy[A: Value](ir: IR[A]): IRs[A] = new IRs(ir, mutable.ListBuffer())
