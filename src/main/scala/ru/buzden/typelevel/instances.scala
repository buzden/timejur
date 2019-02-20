package ru.buzden.typelevel

import cats.Monad
import singleton.ops.+

object instances {
  implicit val unitHasIndexingMonoid: IndexingMonoid[Unit] = new IndexingMonoid[Unit] {
    override type Empty = Unit
    override def empty: Empty = ()

    override type |+|[A, B] = Unit
    override def combine[A <: Unit, B <: Unit]: A |+| B = ()
  }

  implicit val intHasIndexingMonoid: IndexingMonoidZZ[Int] = new IndexingMonoidZZ[Int] {
    override type Empty = 0
    override def empty: Empty = 0

    type ZZ[A, B] = A + B

    override type |+|[A <: Int, B <: Int] = (A + B)#OutInt
    override def combineZZ[A <: Int, B <: Int](implicit p: A + B): A |+| B = p.value.asInstanceOf
  }

  type I2A[M[_], A, B] = M[A]
  implicit def monadIsIndexedMonad[I: IndexingMonoidZZ, M[_]: Monad]: IndexedMonad[I, I2A[M, ?, ?]] = new IndexedMonad[I, I2A[M, ?, ?]] {
    override val im: IndexingMonoidZZ[I] = implicitly

    override def pure[A](a: A): M[A] = Monad[M].pure(a)

    override def flatMap[A, I_A <: I, B, I_B <: I](fa: M[A])(f: A => M[B]): M[B] =
      Monad[M].flatMap(fa)(f)
  }
}
