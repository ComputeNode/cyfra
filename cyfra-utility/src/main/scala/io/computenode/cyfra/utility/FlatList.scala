package io.computenode.cyfra.utility

object FlatList:
  def apply[A](args: A | List[A]*): List[A] = args
    .flatMap:
      case v: A        => List(v)
      case vs: List[A] => vs
    .toList
