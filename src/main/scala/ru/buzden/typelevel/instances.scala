package ru.buzden.typelevel

import cats.Monad
import singleton.ops.+

object instances {
  case class NewTypeUnit(u: Unit) extends AnyVal
  implicit val newTypeUnit: NewTypeUnit = NewTypeUnit(())
  type TwoHoleUnit[A, B] = NewTypeUnit
  implicit val unitHasIndexingMonoid: IndexingMonoid[Unit, TwoHoleUnit] = new IndexingMonoid[Unit, TwoHoleUnit] {
    val u: X[Unit] = ().asInstanceOf[X[Unit]] // I don't know why I need to coerce here.

    override type Empty = X[Unit]
    override def empty: Empty = u

    override type |+|[A, B] = X[Unit]
    override def combine[A <: X[Unit], B <: X[Unit]](implicit i: TwoHoleUnit[A, B]): A |+| B = u
  }

  implicit val intHasIndexingMonoid: IndexingMonoid[Int, +] = new IndexingMonoid[Int, +] {
    override type Empty = 0
    override def empty: Empty = 0

    override type |+|[A <: X[Int], B <: X[Int]] = (A + B)#OutInt
    override def combine[A <: X[Int], B <: X[Int]](implicit p: A + B): A |+| B = p.value.asInstanceOf
  }

  type I2A[M[_], A, B, C] = M[A]
  implicit def monadIsIndexedMonad[I, M[_]: Monad, Z[_, _]](implicit im2: IndexingMonoid[I, Z]): IndexedMonad[I, I2A[M, ?, ?, ?]] = new IndexedMonad[I, I2A[M, ?, ?, ?]] {
    type ZZ[A, B] = Z[A, B]
    override val im: IndexingMonoid[I, Z] = im2

    override def pure[A](a: A): M[A] = Monad[M].pure(a)

    override def flatMap[A, I_A <: X[I], B, I_B <: X[I]](fa: M[A])(f: A => M[B]): M[B] =
      Monad[M].flatMap(fa)(f)
  }
}
