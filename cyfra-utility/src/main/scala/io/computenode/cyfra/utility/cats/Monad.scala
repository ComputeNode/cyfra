package io.computenode.cyfra.utility.cats

trait Monad[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def pure[A](x: A): F[A]
