package io.computenode.cyfra.spirv

import io.computenode.cyfra.dsl.Expression.{E, FunctionCall}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

import scala.collection.mutable
import scala.quoted.Expr

private[cyfra] object BlockBuilder:

  def buildBlock(tree: E[?], providedExprIds: Set[Int] = Set.empty): List[E[?]] =
    val allVisited = mutable.Map[Int, E[?]]()
    val inDegrees = mutable.Map[Int, Int]().withDefaultValue(0)
    val q = mutable.Queue[E[?]]()
    q.enqueue(tree)
    allVisited(tree.treeid) = tree

    while q.nonEmpty do
      val curr = q.dequeue()
      val children = curr.exprDependencies.filterNot(child => providedExprIds.contains(child.treeid))
      children.foreach: child =>
        val childId = child.treeid
        inDegrees(childId) += 1
        if !allVisited.contains(childId) then
          allVisited(childId) = child
          q.enqueue(child)

    val l = mutable.ListBuffer[E[?]]()
    val roots = mutable.Queue[E[?]]()
    allVisited.values.foreach: node =>
      if inDegrees(node.treeid) == 0 then roots.enqueue(node)

    while roots.nonEmpty do
      val curr = roots.dequeue()
      l += curr
      val children = curr.exprDependencies.filterNot(child => providedExprIds.contains(child.treeid))
      children.foreach: child =>
        val childId = child.treeid
        inDegrees(childId) -= 1
        if inDegrees(childId) == 0 then roots.enqueue(child)

    if inDegrees.valuesIterator.exists(_ != 0) then throw new IllegalStateException("Cycle detected in the expression graph: ")
    l.toList.reverse
