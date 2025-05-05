package io.computenode.cyfra.spirv

import io.computenode.cyfra.dsl.Control.Scope
import io.computenode.cyfra.dsl.Expression.{E, FunctionCall}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

import scala.collection.mutable
import scala.quoted.Expr


private[cyfra] object BlockBuilder:

  def transformFnCall[T <: Value: Tag](tree: E[T], enclosingPure: Option[Source.Pure], lastPure: Option[Source.Pure]): E[T] =
    enclosingPure match
      case Some(p) if lastPure.isEmpty
            || (lastPure.get.identifier != p.identifier
            && !lastPure.get.params.map(_.treeid).contains(tree.treeid)) =>
        val funCallParams = p.params
        val call = FunctionCall[T](p.identifier, Scope(tree), funCallParams, tree.treeid)(using tree.tag)
        // todo set call.of
        call
      case _ =>
        tree

  def buildBlock(tree: E[_], inEnclosing: Source.Enclosing): List[E[_]] =
    val allVisited = mutable.Map[Int, E[_]]()
    val inDegrees = mutable.Map[Int, Int]().withDefaultValue(0)
    val q = mutable.Queue[E[_]]()
    // todo detect when inEnclosing != tree.enclosing

    val treeEnclosing: Option[Source.Pure] = tree.of match
      case Some(e: Source.Pure) =>
        Some(e)
      case _ =>
        None

    val transformations = mutable.Map[Int, E[_]]()
    val transformedTree = transformFnCall(tree, treeEnclosing, None)(using tree.tag)
    transformations(tree.treeid) = transformedTree
    q.enqueue(transformedTree)
    allVisited(transformedTree.treeid) = transformedTree

    val resolvedEnclosing = mutable.Map[Int, Option[Source.Pure]](tree.treeid -> None)


    //debug
    val visitedFrom = mutable.Map[Int, List[E[_]]]().withDefaultValue(Nil)

    while q.nonEmpty do
      val curr = q.dequeue()
      val children = curr.exprDependencies
      children.foreach: child =>

        val enclosing = child.of match
          case Some(e) =>
            e.source.enclosing match
              case e: Source.Pure =>
                Some(e)
              case _ =>
                resolvedEnclosing(curr.treeid)
          case _ =>
            resolvedEnclosing(curr.treeid)

        resolvedEnclosing(child.treeid) = enclosing

        val transformedChild = transformFnCall(child, enclosing, resolvedEnclosing(curr.treeid))(using child.tag)
        transformations(child.treeid) = transformedChild

        val childId = transformedChild.treeid
        inDegrees(childId) += 1
        visitedFrom(childId) = curr :: visitedFrom(childId)
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
      val transformedCurr = transformations(curr.treeid)
      l += transformedCurr
      transformedCurr.exprDependencies.foreach: child =>
        val childId = child.treeid
        inDegrees(childId) -= 1
        if inDegrees(childId) == 0 then
          roots.enqueue(child)

    if inDegrees.valuesIterator.exists(_ != 0) then
      val cycleNode = allVisited.values.filter(e => inDegrees(e.treeid) != 0).toList.head
      def resolveCycle(current: E[_], acc: List[E[_]]): Option[List[E[_]]] =
        if acc.contains(current) then
          Some(acc :+ current)
        else if current.exprDependencies.isEmpty then
          None
        else
          current.exprDependencies.view
            .flatMap(resolveCycle(_, acc :+ current)).headOption
              
      val cycle = resolveCycle(cycleNode, List())
      val cyclePretty = cycle match
        case Some(c) =>
          c.mkString(" -> ")
        case None =>
          "unknown"
          
      throw new IllegalStateException("Cycle detected in the expression graph involving " + cyclePretty)
    l.toList.reverse
