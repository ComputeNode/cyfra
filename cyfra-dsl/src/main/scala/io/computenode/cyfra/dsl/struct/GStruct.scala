package io.computenode.cyfra.dsl.struct

import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

import scala.compiletime.*
import scala.deriving.Mirror

abstract class GStruct[T <: GStruct[T]: {Tag, GStructSchema}] extends Value with Product:
  self: T =>
  private[cyfra] var _schema: GStructSchema[T] = summon[GStructSchema[T]] // a nasty hack
  def schema: GStructSchema[T] = _schema
  lazy val tree: E[T] =
    schema.tree(self)
  override protected def init(): Unit = ()
  private[dsl] var _name = Source("Unknown")
  override def source: Source = _name

object GStruct:
  case class Empty(_placeholder: Int32 = 0) extends GStruct[Empty]

  object Empty:
    given GStructSchema[Empty] = GStructSchema.derived

  case class ComposeStruct[T <: GStruct[?]: Tag](fields: List[Value], resultSchema: GStructSchema[T]) extends Expression[T]

  case class GetField[S <: GStruct[?]: GStructSchema, T <: Value: Tag](struct: E[S], fieldIndex: Int) extends Expression[T]:
    val resultSchema: GStructSchema[S] = summon[GStructSchema[S]]

  given [T <: GStruct[T]: GStructSchema]: GStructConstructor[T] with
    def schema: GStructSchema[T] = summon[GStructSchema[T]]

    def fromExpr(expr: E[T])(using Source): T = schema.fromTree(expr)
