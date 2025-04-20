package io.computenode.cyfra.spirv

import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.Value

import scala.collection.mutable
import scala.quoted.Expr


private[cyfra] object ScopeBuilder:

  def buildScope(tree: E[_]): List[E[_]] =
    val allVisited = mutable.Map[Int, E[_]]()
    val inDegrees = mutable.Map[Int, Int]().withDefaultValue(0)
    val q = mutable.Queue[E[_]]()
    q.enqueue(tree)
    allVisited(tree.treeid) = tree

    while q.nonEmpty do
      val curr = q.dequeue()
      val children = curr.exprDependencies
      children.foreach: child =>
        val childId = child.treeid
        inDegrees(childId) += 1
        if !allVisited.contains(childId) then
          allVisited(childId) = child
          q.enqueue(child)

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

