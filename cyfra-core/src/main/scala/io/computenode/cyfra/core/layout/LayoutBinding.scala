package io.computenode.cyfra.core.layout

import io.computenode.cyfra.dsl.binding.GBinding

import scala.Tuple.*
import scala.compiletime.{constValue, erasedValue, error}
import scala.deriving.Mirror

trait LayoutBinding[L <: Layout]:
  def fromBindings(bindings: Seq[GBinding[?]]): L
  def toBindings(layout: L): Seq[GBinding[?]]

object LayoutBinding:
  inline given derived[L <: Layout](using m: Mirror.ProductOf[L]): LayoutBinding[L] =
    allElementsAreBindings[m.MirroredElemTypes, m.MirroredElemLabels]()
    val size = constValue[Size[m.MirroredElemTypes]]
    val constructor = m.fromProduct
    new DerivedLayoutBinding[L](size, constructor)

  // noinspection NoTailRecursionAnnotation
  private inline def allElementsAreBindings[Types <: Tuple, Names <: Tuple](): Unit =
    inline erasedValue[Types] match
      case _: EmptyTuple         => ()
      case _: (GBinding[?] *: t) => allElementsAreBindings[t, Tail[Names]]()
      case _                     =>
        val name = constValue[Head[Names]]
        error(s"$name is not a GBinding, all elements of a Layout must be GBindings")

  class DerivedLayoutBinding[L <: Layout](size: Int, constructor: Product => L) extends LayoutBinding[L]:
    override def fromBindings(bindings: Seq[GBinding[?]]): L =
      assert(bindings.size == size, s"Expected $size) bindings, got ${bindings.size}")
      constructor(Tuple.fromArray(bindings.toArray))
    override def toBindings(layout: L): Seq[GBinding[?]] =
      layout.asInstanceOf[Product].productIterator.map(_.asInstanceOf[GBinding[?]]).toSeq
