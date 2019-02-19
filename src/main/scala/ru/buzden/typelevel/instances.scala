package ru.buzden.typelevel

import cats.Monad
import singleton.ops.+

object instances {
  implicit val unitHasIndexingMonoid: IndexingMonoid[Unit] = new IndexingMonoid[Unit] with SimpleIndexingSemigroup[Unit] {
    val u: X[Unit] = ().asInstanceOf[X[Unit]] // I don't know why I need to coerce here.

    override type Empty = X[Unit]
    override def empty: Empty = u

    override type |+|[A, B] = X[Unit]
    override def combine[A <: X[Unit], B <: X[Unit]]: A |+| B = u
  }

  implicit val intHasIndexingMonoid: IndexingMonoid[Int] = new IndexingMonoid[Int] {
    override type Empty = 0
    override def empty: Empty = 0

    type ZZ[A, B] = A + B

    override type |+|[A <: X[Int], B <: X[Int]] = (A + B)#OutInt
    override def combine[A <: X[Int], B <: X[Int]](implicit p: A + B): A |+| B = p.value.asInstanceOf
  }

  type I2A[M[_], A, B, C] = M[A]
  implicit def monadIsIndexedMonad[I: IndexingMonoid, M[_]: Monad]: IndexedMonad[I, I2A[M, ?, ?, ?]] = new IndexedMonad[I, I2A[M, ?, ?, ?]] {
    override val im: IndexingMonoid[I] = implicitly

    override def pure[A](a: A): M[A] = Monad[M].pure(a)

    override def flatMap[A, I_A <: X[I], B, I_B <: X[I]](fa: M[A])(f: A => M[B]): M[B] =
      Monad[M].flatMap(fa)(f)
  }
}
