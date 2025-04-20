package io.computenode.cyfra.dsl.macros

import Source.Enclosing

import scala.quoted.*
import io.computenode.cyfra.dsl.Value

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

  case object NonPure extends Enclosing
  case class Pure(name: String) extends Enclosing


  inline implicit def generate: Source = ${ sourceImpl }

  def sourceImpl(using Quotes): Expr[Source] = {
    import quotes.reflect.*

    val path = enclosingMethod
    val name = valueName
    '{Source(${name}, ${path})}
  }

  def valueName(using Quotes): Expr[String] = 
    import quotes.reflect._
    val owner = actualOwner(Symbol.spliceOwner)
    val simpleName = Util.getName(owner)
    Expr(simpleName)
  

  def enclosingMethod(using Quotes): Expr[Enclosing] = 
    import quotes.reflect._
    val ownerDef = findOwner(Symbol.spliceOwner, owner0 => Util.isSynthetic(owner0) || Util.getName(owner0) == "ev" || !owner0.isDefDef)
    ownerDef.tree match {
      case dd: DefDef if isPure(dd) => 
        val name = Util.getName(ownerDef)
        '{ Pure(${Expr(name)}) }
      case _ => '{ NonPure }
    }
    

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

  def findOwner(using Quotes)(owner: quotes.reflect.Symbol, skipIf: quotes.reflect.Symbol => Boolean): quotes.reflect.Symbol = {
    var owner0 = owner
    while(skipIf(owner0)) owner0 = owner0.owner
    owner0
  }

  def actualOwner(using Quotes)(owner: quotes.reflect.Symbol): quotes.reflect.Symbol =
    findOwner(owner, owner0 => Util.isSynthetic(owner0) || Util.getName(owner0) == "ev")

  def nonMacroOwner(using Quotes)(owner: quotes.reflect.Symbol): quotes.reflect.Symbol =
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

  def enclosingValOrDef(using Quotes)(filter: quotes.reflect.Symbol => Boolean): String = {
    import quotes.reflect._

    var current = Symbol.spliceOwner
    current = actualOwner(current)
    var path = List.empty[Chunk]
    while(current != Symbol.noSymbol && current != defn.RootPackage && current != defn.RootClass){
      if (filter(current)) {

        val chunk = current match {
          case sym if
            (sym.isValDef && !sym.owner.paramSymss.flatten.contains(sym)) || sym.isDefDef => 
              println(sym.owner.paramSymss)
              Chunk.ValVarLzyDef.apply
          case sym if
            sym.isPackageDef ||
            sym.moduleClass != Symbol.noSymbol => Chunk.PkgObj.apply
          case sym if sym.isClassDef => Chunk.ClsTrt.apply
          case _ => Chunk.PkgObj.apply
        }

        path = chunk(Util.getName(current).stripSuffix("$")) :: path
      }
      current = current.owner
    }
    path.map{
      case Chunk.PkgObj(s) => adjustName(s) + "."
      case Chunk.ClsTrt(s) => adjustName(s) + "#"
      case Chunk.ValVarLzyDef(s) => adjustName(s) + " "
    }.mkString.dropRight(1)
  }

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
