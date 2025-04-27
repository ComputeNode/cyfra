package io.computenode.cyfra.dsl.macros

import Source.Enclosing

import scala.quoted.*
import io.computenode.cyfra.dsl.{Expression, Value}

// Part of this file is copied from lihaoyi's sourcecode library: https://github.com/com-lihaoyi/sourcecode

case class Source(
  name: String,
  enclosing: Enclosing
)

object Source:

  /**
   * Value is instatiated at position P
   * If P is inside a function directly, without any other templates in the way, then this function is Enclosing
   * Enclosing is pure if:
   *   - Has only Values as parameters
   *   - Returns a Value
   *   (- Does not call non-pure functions*)? For now maybe no?
   * if it has @gfun annotation, then the last condition is not required, and other conditions are validated,
   * error is thrown when function is not pure
   */
  

  sealed trait Enclosing

  case class NonPure(shortName: String, fullName: String) extends Enclosing
  case class Pure(shortName: String, fullName: String, params: List[Value]) extends Enclosing:
    val identifier: PureIdentifier = PureIdentifier(shortName, fullName)
  case class PureIdentifier(shortName: String, fullName: String)
  case object Pass extends Enclosing

  inline implicit def generate: Source = ${ sourceImpl }

  def sourceImpl(using Quotes): Expr[Source] = {
    import quotes.reflect.*

    val path = enclosingMethod
    val name = valueName
    '{Source(${name}, ${path})}
  }

  def valueName(using Quotes): Expr[String] =
    import quotes.reflect._
    val ownerOpt = actualOwner(Symbol.spliceOwner)
    ownerOpt match
      case Some(owner) =>
        val simpleName = Util.getName(owner)
        Expr(simpleName)
      case None =>
        Expr("unknown")
  
  def enclosingMethod(using Quotes): Expr[Enclosing] =
    import quotes.reflect.*
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
            '{ Pure(${ Expr(name) }, ${ Expr(ownerName) }, ${ paramList }) }
          case _ =>
            '{ NonPure(${ Expr(name) }, ${ Expr(ownerName) }) }
        }
      case None => '{ NonPure("unknown", "unknown") }

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
    

  def findOwner(using Quotes)(owner: quotes.reflect.Symbol, skipIf: quotes.reflect.Symbol => Boolean): Option[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    var owner0 = owner
    while(skipIf(owner0))
      if owner0 == Symbol.noSymbol then
        return None
      owner0 = owner0.owner
    Some(owner0)
  }

  def actualOwner(using Quotes)(owner: quotes.reflect.Symbol): Option[quotes.reflect.Symbol] =
    findOwner(owner, owner0 => Util.isSynthetic(owner0) || Util.getName(owner0) == "ev")

  def nonMacroOwner(using Quotes)(owner: quotes.reflect.Symbol): Option[quotes.reflect.Symbol] =
    findOwner(owner, owner0 => { owner0.flags.is(quotes.reflect.Flags.Macro) && Util.getName(owner0) == "macro"})

  private def adjustName(s: String): String =
    // Required to get the same name from dotty
    if (s.startsWith("<local ") && s.endsWith("$>"))
      s.stripSuffix("$>") + ">"
    else
      s

  sealed trait Chunk
  object Chunk:
    case class PkgObj(name: String) extends Chunk
    case class ClsTrt(name: String) extends Chunk
    case class ValVarLzyDef(name: String) extends Chunk


object Util:
  def isSynthetic(using Quotes)(s: quotes.reflect.Symbol) =
    isSyntheticAlt(s)

  def isSyntheticAlt(using Quotes)(s: quotes.reflect.Symbol) = {
    import quotes.reflect._
    s.flags.is(Flags.Synthetic) || s.isClassConstructor || s.isLocalDummy || isScala2Macro(s) || s.name.startsWith("x$proxy")
  }
  def isScala2Macro(using Quotes)(s: quotes.reflect.Symbol) = {
    import quotes.reflect._
    (s.flags.is(Flags.Macro) && s.owner.flags.is(Flags.Scala2x)) ||
      (s.flags.is(Flags.Macro) && !s.flags.is(Flags.Inline))
  }
  def isSyntheticName(name: String) = {
    name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name == "$anonfun" || name == "macro"
  }
  def getName(using Quotes)(s: quotes.reflect.Symbol) = {
    s.name.trim
      .stripSuffix("$") // meh
  }
