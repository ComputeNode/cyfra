package io.computenode.cyfra.dsl.struct

import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.struct.GStruct.*
import izumi.reflect.Tag

import scala.compiletime.{constValue, erasedValue, error, summonAll}
import scala.deriving.Mirror

case class GStructSchema[T <: GStruct[?]: Tag](fields: List[(String, FromExpr[?], Tag[?])], dependsOn: Option[E[T]], fromTuple: (Tuple, Source) => T):
  given GStructSchema[T] = this
  val structTag = summon[Tag[T]]

  def tree(t: T): E[T] =
    dependsOn match
      case Some(dep) => dep
      case None      =>
        ComposeStruct[T](t.productIterator.toList.asInstanceOf[List[Value]], this)

  def create(values: List[Value], schema: GStructSchema[T])(using name: Source): T =
    val valuesTuple = Tuple.fromArray(values.toArray)
    val newStruct = fromTuple(valuesTuple, name)
    newStruct._schema = schema.asInstanceOf
    newStruct.tree.of = Some(newStruct)
    newStruct

  def fromTree(e: E[T])(using Source): T =
    create(
      fields.zipWithIndex.map { case ((_, fromExpr, tag), i) =>
        fromExpr
          .asInstanceOf[FromExpr[Value]]
          .fromExpr(GetField[T, Value](e, i)(using this, tag.asInstanceOf[Tag[Value]]).asInstanceOf[E[Value]])
      },
      this.copy(dependsOn = Some(e)),
    )

  val gStructTag = summon[Tag[GStruct[?]]]

object GStructSchema:
  type TagOf[T] = Tag[T]
  type FromExprOf[T] = T match
    case Value => FromExpr[T]
    case _     => Nothing

  inline given derived[T <: GStruct[T]: Tag](using m: Mirror.Of[T]): GStructSchema[T] =
    inline m match
      case m: Mirror.ProductOf[T] =>
        // quick prove that all fields <:< value
        summonAll[Tuple.Map[m.MirroredElemTypes, [f] =>> f <:< Value]]
        // get (name, tag) pairs for all fields
        val elemTags: List[Tag[?]] = summonAll[Tuple.Map[m.MirroredElemTypes, TagOf]].toList.asInstanceOf[List[Tag[?]]]
        val elemFromExpr: List[FromExpr[?]] = summonAll[Tuple.Map[m.MirroredElemTypes, [f] =>> FromExprOf[f]]].toList.asInstanceOf[List[FromExpr[?]]]
        val elemNames: List[String] = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
        val elements = elemNames.lazyZip(elemFromExpr).lazyZip(elemTags).toList
        GStructSchema[T](
          elements,
          None,
          (tuple, name) => {
            val inst = m.fromTuple.asInstanceOf[Tuple => T].apply(tuple)
            inst._name = name
            inst
          },
        )
      case _ => error("Only case classes are supported as GStructs")

  private inline def constValueTuple[T <: Tuple]: T =
    (inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts)  => constValue[t] *: constValueTuple[ts]
    ).asInstanceOf[T]
