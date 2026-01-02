package io.computenode.cyfra.core.layout

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.compiletime.{error, summonAll}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

case class LayoutStruct[T <: Layout: Tag](layoutRef: T)
