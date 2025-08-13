package io.computenode.cyfra.vulkan.compute

/** @author
  *   MarconZet Created 25.04.2020
  */
private[cyfra] case class LayoutInfo(sets: Seq[LayoutSet])
private[cyfra] case class LayoutSet(id: Int, bindings: Seq[Binding])
private[cyfra] case class Binding(id: Int, size: LayoutElementSize)
private[cyfra] sealed trait LayoutElementSize
private[cyfra] case class InputBufferSize(elemSize: Int) extends LayoutElementSize
private[cyfra] case class UniformSize(size: Int) extends LayoutElementSize
