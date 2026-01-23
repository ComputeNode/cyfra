package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.{IntegerType, Mat, RuntimeArray, Vec}

import scala.quoted.{Expr, Quotes, Type}

sealed trait Focus[T: Value]

trait FocusRoot[T: Value] extends Focus[T]

case class FocusConstant[Parent: Value, T: Value](parent: Focus[Parent], value: Int) extends Focus[T]

case class FocusDynamic[Parent: Value, T: Value](parent: Focus[Parent], value: IntegerType) extends Focus[T]

object Focus:
  trait FocusContext:
    extension [To: Value, I <: IntegerType: Value](from: RuntimeArray[To])
      def at(index: I): To = scala.sys.error("method can only be used inside focus lambda")

  extension [From: Value, To: Value](from: Focus[From])
    transparent inline def focus(inline lambda: FocusContext ?=> From => To): Focus[To] =
      ${ focusImpl[From, To]('from, 'lambda) }

  def focusImpl[From: Type, To: Type](from: Expr[Focus[From]], lambda: Expr[FocusContext ?=> From => To])(using quotes: Quotes): Expr[Focus[To]] =
    import quotes.reflect.*
    given Printer[Tree] = Printer.TreeCode

    report.info(from.show)
    report.info(lambda.show)
    '{ null.asInstanceOf[Focus[To]] }
