package io.computenode.cyfra.core.layout

import io.computenode.cyfra.core.layout.Layout.IllegalLayoutElement
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer}

trait Layout:
  self: Product =>

  lazy val bindings: Seq[GBinding[?]] =
    validateElements()
    self.productIterator
      .map(_.asInstanceOf[GBinding[?]])
      .toSeq

  private def validateElements(): Unit =
    val invalidIndex = self.productIterator.indexWhere(!_.isInstanceOf[GBinding[?]])
    if invalidIndex != -1 then throw IllegalLayoutElement(self.productElementName(invalidIndex))

object Layout:

  case class IllegalLayoutElement(name: String) extends Exception(s"All Layout members must be GBindings, $name is not")
