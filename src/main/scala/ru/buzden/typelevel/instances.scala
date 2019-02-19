package ru.buzden.typelevel

import cats.Monad

object instances {
  implicit val unitHasIndexingMonoid: IndexingMonoid[Unit] = new IndexingMonoid[Unit] {
    val u: X[Unit] = ().asInstanceOf[X[Unit]] // I don't know why I need to coerce here.

    override type Empty = X[Unit]
    override def empty: Empty = u

    override type |+|[A, B] = X[Unit]
    override def combine[A <: X[Unit], B <: X[Unit]]: A |+| B = u
  }

  type I2A[M[_], A, B, C] = M[A]
  implicit def monadIsIndexedMonad[I: IndexingMonoid, M[_]: Monad]: IndexedMonad[I, I2A[M, ?, ?, ?]] = new IndexedMonad[I, I2A[M, ?, ?, ?]] {
    override val im: IndexingMonoid[I] = implicitly

    override def pure[A](a: A): M[A] = Monad[M].pure(a)

    override def flatMap[A, I_A <: X[I], B, I_B <: X[I]](fa: M[A])(f: A => M[B]): M[B] =
      Monad[M].flatMap(fa)(f)
  }
}
