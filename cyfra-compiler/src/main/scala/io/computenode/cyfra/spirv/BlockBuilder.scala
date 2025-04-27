package io.computenode.cyfra.spirv

import io.computenode.cyfra.dsl.Control.Scope
import io.computenode.cyfra.dsl.Expression.{E, FunctionCall}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

import scala.collection.mutable
import scala.quoted.Expr


private[cyfra] object BlockBuilder:

  def transformFnCall[T <: Value: Tag](tree: E[T], enclosing: Source.Enclosing, lastEnclosing: Source.Enclosing): E[T] =
    enclosing match
      case p: Source.Pure if enclosing != lastEnclosing =>
        val funCallParams = p.params
        FunctionCall[T](p.identifier, Scope(tree), funCallParams, tree.treeid)(using tree.tag)
      case _ =>
        tree

  def buildBlock(tree: E[_], inEnclosing: Source.Enclosing): List[E[_]] =
    val allVisited = mutable.Map[Int, E[_]]()
    val inDegrees = mutable.Map[Int, Int]().withDefaultValue(0)
    val q = mutable.Queue[E[_]]()
    // todo detect when inEnclosing != tree.enclosing

    val treeEnclosing = tree.of match
      case Some(e) =>
        e.source.enclosing match
          case e: (Source.Pure | Source.NonPure) =>
            e
          case Source.Pass =>
            inEnclosing
      case _ =>
        inEnclosing

    val transformedTree = transformFnCall(tree, treeEnclosing, inEnclosing)(using tree.tag)
    q.enqueue(transformedTree)
    allVisited(transformedTree.treeid) = transformedTree

    val resolvedEnclosing = mutable.Map[Int, Source.Enclosing](tree.treeid -> treeEnclosing)

    while q.nonEmpty do
      val curr = q.dequeue()
      val children = curr.exprDependencies
      children.foreach: child =>

        val enclosing = child.of match
          case Some(e) =>
            e.source.enclosing match
              case e: (Source.Pure | Source.NonPure) =>
                e
              case Source.Pass =>
                resolvedEnclosing(curr.treeid)
          case _ =>
            resolvedEnclosing(curr.treeid)

        resolvedEnclosing(child.treeid) = enclosing

        val transformedChild = transformFnCall(child, enclosing, resolvedEnclosing(curr.treeid))(using child.tag)
        val childId = transformedChild.treeid
        inDegrees(childId) += 1
        if !allVisited.contains(childId) then
          allVisited(childId) = transformedChild
          q.enqueue(transformedChild)

    val l = mutable.ListBuffer[E[_]]()
    val roots = mutable.Queue[E[_]]()
    allVisited.values.foreach: node =>
      if inDegrees(node.treeid) == 0 then
        roots.enqueue(node)

    while roots.nonEmpty do
      val curr = roots.dequeue()
      l += curr
      curr.exprDependencies.foreach: child =>
        val childId = child.treeid
        inDegrees(childId) -= 1
        if inDegrees(childId) == 0 then
          roots.enqueue(child)

    if inDegrees.valuesIterator.exists(_ != 0) then
      throw new IllegalStateException("Cycle detected in the expression graph: ")
    l.toList.reverse

