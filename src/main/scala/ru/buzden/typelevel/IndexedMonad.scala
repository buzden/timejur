package ru.buzden.typelevel

import cats.Monad

/**
  * Typeclass for the indexed monad with singleton index types.
  *
  * Described indexed monad type is three-holed:
  * - type of value (analogue of usual monad's type parameter);
  * - general type of index (which remains unchanged during operations);
  * - singleton type of particular index value (which changes during operations).
  *
  * The third type argument must be a singleton and must
  * inherit the second type argument at the same time.
  *
  * @tparam I used general type of index
  * @tparam F described three-holed monad type
  */
trait IndexedMonad[I, F[_, J, _ <: X[J]]] {
  val im: IndexingMonoid[I]
  import im._

  def pure[A](a: A): F[A, I, Empty]
  def flatMap[A, I_A <: X[I], B, I_B <: X[I]](fa: F[A, I, I_A])(f: A => F[B, I, I_B]): F[B, I, I_A |+| I_B]
}

object IndexedMonad {
  object syntax {
    implicit class IndexedMonadAnyAOps[A](val a: A) extends AnyVal {
      def pure[I, F[_, _, _]](implicit iM: IndexedMonad[I, F]): F[A, I, iM.im.Empty] = iM.pure(a)
    }

    implicit class IndexedMonadOps[I, F[_, _, _], A, I_A <: X[I]](val fa: F[A, I, I_A]) extends AnyVal {
      def flatMap[B, I_B <: X[I]](f: A => F[B, I, I_B])(implicit iM: IndexedMonad[I, F]): F[B, I, iM.im.|+|[I_A, I_B]] =
        iM.flatMap[A, I_A, B, I_B](fa)(f)
    }
  }

  type I2A[M[_], A, B, C] = M[A]
  implicit def monadIsIndexedMonad[M[_]: Monad]: IndexedMonad[Unit, I2A[M, ?, ?, ?]] = new IndexedMonad[Unit, I2A[M, ?, ?, ?]] {
    override val im: IndexingMonoid[Unit] = implicitly

    override def pure[A](a: A): M[A] = Monad[M].pure(a)

    override def flatMap[A, I_A <: X[Unit], B, I_B <: X[Unit]](fa: M[A])(f: A => M[B]): M[B] =
      Monad[M].flatMap(fa)(f)
  }
}
