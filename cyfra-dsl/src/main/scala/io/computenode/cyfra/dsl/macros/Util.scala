package io.computenode.cyfra.dsl.macros

import scala.quoted.*

object Util:
  def isSynthetic(using Quotes)(s: quotes.reflect.Symbol) =
    isSyntheticAlt(s)

  def isSyntheticAlt(using Quotes)(s: quotes.reflect.Symbol) = {
    import quotes.reflect._
    s.flags.is(Flags.Synthetic) || s.isClassConstructor || s.isLocalDummy || isScala2Macro(s) || s.name.startsWith("x$proxy")
  }
  def isScala2Macro(using Quotes)(s: quotes.reflect.Symbol) = {
    import quotes.reflect._
    (s.flags.is(Flags.Macro) && s.owner.flags.is(Flags.Scala2x)) || (s.flags.is(Flags.Macro) && !s.flags.is(Flags.Inline))
  }
  def isSyntheticName(name: String) =
    name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name == "$anonfun" || name == "macro"
  def getName(using Quotes)(s: quotes.reflect.Symbol) =
    s.name.trim
      .stripSuffix("$") // meh
