package ru.buzden.typelevel

import cats.Monad
import singleton.ops.+

object instances {
  type Unit2H[A, B] = Unit
  implicit val unitHasIndexingMonoid: SimpleIndexingMonoid[Unit, Unit, Unit2H] =
    new SimpleIndexingMonoid[Unit, Unit, Unit2H]

  implicit def unitHasIndexingMonoidEmerger[A <: Unit, B <: Unit]: IndexingMonoidEmerger[Unit, A, B] = new IndexingMonoidEmerger[Unit, A, B] {
    override val proto: unitHasIndexingMonoid.type = unitHasIndexingMonoid
    import proto._

    override def empty: Empty = ()
    override def combine: A |+| B = ()
  }

  type AddInt[A <: Int, B <: Int] = (A + B)#OutInt
  implicit val intHasIndexingMonoid: SimpleIndexingMonoid[Int, 0, AddInt] =
    new SimpleIndexingMonoid[Int, 0, AddInt]

  implicit def intHasIndexingMonoidEmerger[A <: Int, B <: Int](implicit p: A + B): IndexingMonoidEmerger[Int, A, B] = new IndexingMonoidEmerger[Int, A, B] {
    override val proto: intHasIndexingMonoid.type = intHasIndexingMonoid
    import proto._

    override def empty: Empty = 0
    override def combine: A |+| B = p.value.asInstanceOf
  }

  type I2A[M[_], A, B] = M[A]
  implicit def monadIsIndexedMonad[I: IndexingMonoid, M[_]: Monad]: IndexedMonad[I, I2A[M, ?, ?]] = new IndexedMonad[I, I2A[M, ?, ?]] {
    override val im: IndexingMonoid[I] = implicitly

    override def pure[A](a: A): M[A] = Monad[M].pure(a)
    override def flatMap[A, I_A <: I, B, I_B <: I](fa: M[A])(f: A => M[B]): M[B] = Monad[M].flatMap(fa)(f)
  }
}
