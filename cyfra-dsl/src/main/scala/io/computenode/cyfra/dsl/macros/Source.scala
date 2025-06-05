package io.computenode.cyfra.dsl.macros

import scala.quoted.*
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.WeakTag
import izumi.reflect.macrortti.LightTypeTag

// Part of this file is copied from lihaoyi's sourcecode library: https://github.com/com-lihaoyi/sourcecode

case class Source(name: String)

object Source:

  implicit inline def generate: Source = ${ sourceImpl }

  def sourceImpl(using Quotes): Expr[Source] = {
    import quotes.reflect.*
    val name = valueName
    '{ Source(${ name }) }
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

  def findOwner(using Quotes)(owner: quotes.reflect.Symbol, skipIf: quotes.reflect.Symbol => Boolean): Option[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    var owner0 = owner
    while (skipIf(owner0))
      if owner0 == Symbol.noSymbol then return None
      owner0 = owner0.owner
    Some(owner0)
  }

  def actualOwner(using Quotes)(owner: quotes.reflect.Symbol): Option[quotes.reflect.Symbol] =
    findOwner(owner, owner0 => Util.isSynthetic(owner0) || Util.getName(owner0) == "ev")

  def nonMacroOwner(using Quotes)(owner: quotes.reflect.Symbol): Option[quotes.reflect.Symbol] =
    findOwner(owner, owner0 => owner0.flags.is(quotes.reflect.Flags.Macro) && Util.getName(owner0) == "macro")

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
