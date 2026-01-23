package io.computenode.cyfra.utility.cats

import io.computenode.cyfra.utility.cats.Free.*

sealed abstract class Free[S[_], A] extends Product with Serializable:

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

  final def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    FlatMapped(this, f)

  final def foldMap[M[_]](f: FunctionK[S, M])(implicit M: Monad[M]): M[A] = this match
    case Pure(a)          => M.pure(a)
    case Suspend(sa)      => f(sa)
    case FlatMapped(c, g) => M.flatMap(c.foldMap(f))(cc => g(cc).foldMap(f))

object Free:
  final case class Pure[S[_], A](a: A) extends Free[S, A]
  final case class Suspend[S[_], A](a: S[A]) extends Free[S, A]
  final case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]

  def pure[S[_], A](a: A): Free[S, A] = Pure(a)

  def liftF[F[_], A](value: F[A]): Free[F, A] = Suspend(value)
