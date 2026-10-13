package io.computenode.cyfra.utility

object FlatList:
  def apply[A](args: A | List[A] | Option[A]*): List[A] = args
    .flatMap:
      case vs: List[A]     => vs
      case vopt: Option[A] => vopt.toList
      case v: A            => List(v)
    .toList
