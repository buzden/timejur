package ru.buzden.typelevel

import cats.Monad
import singleton.ops.+

object instances {
  implicit val unitHasIndexingMonoid: EmergingIndexingMonoid[Unit] = new EmergingIndexingMonoid[Unit] {
    override type EmptyR[A] = A
    override type CombinationR[A, B, C] = C

    override type Empty = Unit
    override type |+|[A, B] = Unit

    override def empty: Empty = ()
    override def combine[A, B]: A |+| B = ()
  }

  implicit val intHasIndexingMonoid: EmergingIndexingMonoid[Int] = new EmergingIndexingMonoid[Int] {
    override type EmptyR[A] = A
    override type CombinationR[A, B, C] = IFT[A + B, C]

    override type Empty = 0
    override type |+|[A, B] = (A + B)#OutInt

    override def empty: Empty = 0
    override def combine[A, B]: CombinationR[A, B, A |+| B] = IFT { implicit p =>
      p.value.asInstanceOf
    }
  }

  type I2A[M[_], A, I_A] = M[A]
  implicit def monadIsIndexedMonad[I: IndexingMonoid, M[_]: Monad]: IndexedMonad[I, I2A[M, ?, ?]] = new IndexedMonad[I, I2A[M, ?, ?]] {
    override val im: IndexingMonoid[I] = implicitly

    override type PureR[A] = A
    override type FlatMapR[I_A, I_B, C] = C

    override def pure[A](a: A): M[A] = Monad[M].pure(a)
    override def flatMap[A, I_A <: I, B, I_B <: I](fa: M[A])(f: A => M[B]): M[B] = Monad[M].flatMap(fa)(f)
  }
}
