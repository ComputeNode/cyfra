package io.computenode.cyfra.utility

object FlatList:
  def apply[A](args: A | List[A]*): List[A] = args
    .flatMap:
      case vs: List[A] => vs
      case v: A        => List(v)
    .toList
