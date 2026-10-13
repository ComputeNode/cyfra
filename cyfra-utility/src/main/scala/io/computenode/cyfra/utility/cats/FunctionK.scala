package io.computenode.cyfra.utility.cats

trait FunctionK[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]
