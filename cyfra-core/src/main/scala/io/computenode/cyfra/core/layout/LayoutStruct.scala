package io.computenode.cyfra.core.layout

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.compiletime.{error, summonAll}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

case class LayoutStruct[T <: Layout: Tag]( val layoutRef: T, private[cyfra] val elementTypes: List[Tag[?]])

object LayoutStruct:

  inline given derived[T <: Layout: Tag]: LayoutStruct[T] = ${ derivedImpl }

  def derivedImpl[T <: Layout: Type](using quotes: Quotes): Expr[LayoutStruct[T]] = ???
