package io.computenode.cyfra.dsl.macros

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.macros.FnCall.FnIdentifier
import io.computenode.cyfra.dsl.macros.Source.{actualOwner, findOwner}
import izumi.reflect.macrortti.LightTypeTag
import scala.quoted.*


case class FnCall(shortName: String, fullName: String, params: List[Value]):
  def identifier: FnIdentifier = FnIdentifier(shortName, fullName, params.map(_.tree.tag.tag))

object FnCall:

  inline implicit def generate: FnCall = ${ fnCallImpl }

  def fnCallImpl(using Quotes): Expr[FnCall] = {
    import quotes.reflect.*
    resolveFnCall
  }

  case class FnIdentifier(shortName: String, fullName: String, args: List[LightTypeTag])

  def resolveFnCall(using Quotes): Expr[FnCall] =
    import quotes.reflect.*
    val applyOwner = Symbol.spliceOwner.owner
    quotes.reflect.report.info(applyOwner.toString)
    val ownerDefOpt = findOwner(Symbol.spliceOwner, owner0 => Util.isSynthetic(owner0) || Util.getName(owner0) == "ev" || !owner0.isDefDef)
    ownerDefOpt match
      case Some(ownerDef) =>
        val name = Util.getName(ownerDef)
        val ddOwner = actualOwner(ownerDef)
        val ownerName = ddOwner.map(d => d.fullName).getOrElse("unknown")
        ownerDef.tree match {
          case dd: DefDef if isPure(dd) =>
            val paramTerms: List[Term] = for {
              paramGroup <- dd.paramss
              param <- paramGroup.params
            } yield Ref(param.symbol)
            val paramExprs: List[Expr[Value]] = paramTerms.map(_.asExpr.asInstanceOf[Expr[Value]])
            val paramList = Expr.ofList(paramExprs)
            '{ FnCall(${ Expr(name) }, ${ Expr(ownerName) }, ${ paramList }) }
          case _ =>
            quotes.reflect.report.errorAndAbort(s"Expected pure function. Found: ${ownerDef}")
        }
      case None => quotes.reflect.report.errorAndAbort(s"Expected pure function")

  def isPure(using Quotes)(defdef: quotes.reflect.DefDef): Boolean =
    import quotes.reflect._
    val returnType = defdef.returnTpt.tpe
    val paramSets = defdef.termParamss
    if paramSets.length > 1 then return false
    val params = paramSets.headOption.map(_.params).getOrElse(Nil)
    val valueType = TypeRepr.of[Value]
    val areParamsPure = params
      .map(_.tpt.tpe)
      .forall(tpe => tpe <:< valueType)
    val isReturnPure = returnType <:< valueType
    areParamsPure && isReturnPure