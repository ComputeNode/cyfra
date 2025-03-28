package io.computenode.cyfra.api

/**
 * Represents layout information for descriptor sets and bindings
 */
case class LayoutInfo(sets: Seq[LayoutSet]) {
  /**
   * Creates a new layout with an additional set
   * @param set The set to add
   * @return A new LayoutInfo instance
   */
  def withSet(set: LayoutSet): LayoutInfo = 
    LayoutInfo(sets :+ set)
    
  /**
   * Creates a new standard input-output layout
   * @param inputBindingCount Number of input bindings
   * @param outputBindingCount Number of output bindings
   * @param elementSize Size of each element in bytes
   * @return A new LayoutInfo instance
   */
  def withIOLayout(inputBindingCount: Int, outputBindingCount: Int, elementSize: Int): LayoutInfo = {
    val inputSet = LayoutSet(0, (0 until inputBindingCount).map(id => Binding(id, elementSize)))
    val outputSet = LayoutSet(1, (0 until outputBindingCount).map(id => Binding(id, elementSize)))
    LayoutInfo(Seq(inputSet, outputSet))
  }
}

object LayoutInfo {
  /**
   * Creates a standard input-output layout
   * @param inputBindingCount Number of input bindings
   * @param outputBindingCount Number of output bindings
   * @param elementSize Size of each element in bytes
   * @return A new LayoutInfo instance
   */
  def standardIOLayout(inputBindingCount: Int, outputBindingCount: Int, elementSize: Int): LayoutInfo = {
    val inputSet = LayoutSet(0, (0 until inputBindingCount).map(id => Binding(id, elementSize)))
    val outputSet = LayoutSet(1, (0 until outputBindingCount).map(id => Binding(id, elementSize)))
    LayoutInfo(Seq(inputSet, outputSet))
  }
}

/**
 * Represents a descriptor set with bindings
 */
case class LayoutSet(id: Int, bindings: Seq[Binding]) {
  /**
   * Creates a new set with an additional binding
   * @param binding The binding to add
   * @return A new LayoutSet instance
   */
  def withBinding(binding: Binding): LayoutSet =
    LayoutSet(id, bindings :+ binding)
}

/**
 * Represents a binding with an ID and size
 */
case class Binding(id: Int, size: Int)