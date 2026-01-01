package io.computenode.cyfra.compiler.ir

import IR.*
import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.ir.IRs.*
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.compiler.spirv.Opcodes.Op
import io.computenode.cyfra.utility.cats.{FunctionK, ~>}

import scala.collection.mutable

case class IRs[A: Value](result: IR[A], body: List[IR[?]]):

  def prepend(ir: IR[?]): IRs[A] = IRs(result, ir :: body)

  def filterOut(p: IR[?] => Boolean): (IRs[A], List[IR[?]]) =
    val removed = mutable.Buffer.empty[IR[?]]
    val next = flatMapReplace:
      case x if p(x) =>
        removed += x
        IRs.proxy(x)(using x.v)
      case x => IRs(x)(using x.v)
    (next, removed.toList)

  def flatMapReplace(f: IR[?] => IRs[?]): IRs[A] = flatMapReplace()(f)

  def flatMapReplace(enterControlFlow: Boolean = true)(f: IR[?] => IRs[?]): IRs[A] =
    flatMapReplaceImpl(f, mutable.Map.empty, enterControlFlow)

  private def flatMapReplaceImpl(f: IR[?] => IRs[?], replacements: mutable.Map[Int, RefIR[?]], enterControlFlow: Boolean): IRs[A] =
    val nBody = body.flatMap: (v: IR[?]) =>
      val next = v match
        case b: Branch[a] if enterControlFlow =>
          given Value[a] = b.v
          val Branch(cond, ifTrue, ifFalse, t) = b
          val nextT = ifTrue.flatMapReplaceImpl(f, replacements, enterControlFlow)
          val nextF = ifFalse.flatMapReplaceImpl(f, replacements, enterControlFlow)
          Branch[a](cond, nextT, nextF, t)
        case Loop(mainBody, continueBody, b, c) if enterControlFlow =>
          val nextM = mainBody.flatMapReplaceImpl(f, replacements, enterControlFlow)
          val nextC = continueBody.flatMapReplaceImpl(f, replacements, enterControlFlow)
          Loop(nextM, nextC, b, c)
        case other => other
      val subst = next.substitute(replacements)
      val IRs(result, body) = f(subst)
      v match
        case v: RefIR[?] => replacements(v.id) = result.asInstanceOf[RefIR[?]]
        case _           => ()
      body

    // We neet to watch out for forward references
    val codesWithLabels = Set(Op.OpLoopMerge, Op.OpSelectionMerge, Op.OpBranch, Op.OpBranchConditional, Op.OpSwitch)
    val nextBody = nBody.map:
      case x @ IR.SvInst(code, _) if codesWithLabels(code) => x.substitute(replacements) // all ops that point to labels
      case x @ IR.SvRef(Op.OpPhi, _, args)                 =>
        // this can contain a cyclical forward reference, let's crash if we may have to handle it
        val safe = args.forall:
          case ref: RefIR[?] => replacements.get(ref.id).forall(_.id == ref.id)
          case _             => true
        if safe then x else throw CompilationException("Forward reference detected in OpPhi")
      case other => other

    val nextResult = replacements.getOrElse(result.id, result).asInstanceOf[IR[A]]
    IRs(nextResult, nextBody)

object IRs:
  def apply[A: Value](ir: IR[A]): IRs[A] = new IRs(ir, List(ir))
  def proxy[A: Value](ir: IR[A]): IRs[A] = new IRs(ir, List())
