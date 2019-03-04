package ru.buzden.iz

import cats.Monad
import singleton.ops.+

object examples {
  implicit val unitHasIndexingMonoid: TwoFacedMonoid[Unit] = new TwoFacedMonoid[Unit] {
    override type EmptyR[A] = A
    override type CombineR[A, B, C] = C

    override type Empty = Unit
    override type |+|[A, B] = Unit

    override def empty: Empty = ()
    override def combine[A, B](a: A, b: B): A |+| B = ()
  }

  implicit val intHasIndexingMonoid: TwoFacedMonoid[Int] = new TwoFacedMonoid[Int] {
    override type EmptyR[A] = A
    override type CombineR[A, B, C] = IFT[A + B, C]

    override type Empty = 0
    override type |+|[A, B] = (A + B)#OutInt

    override def empty: Empty = 0
    override def combine[A, B](a: A, b: B): CombineR[A, B, A |+| B] = IFT { implicit p =>
      p.value.asInstanceOf
    }
  }

  type I2A[M[_], A, I_A] = M[A]
  implicit def monadIsIndexedMonad[I: TypeLevelMonoid, M[_]: Monad]: IzMonad[I, I2A[M, ?, ?]] = new IzMonad[I, I2A[M, ?, ?]] {
    override val im: TypeLevelMonoid[I] = implicitly

    override type PureR[A] = A
    override type FlatMapR[I_A, I_B, C] = C

    override def pure[A](a: A): M[A] = Monad[M].pure(a)
    override def flatMap[A, I_A <: I, B, I_B <: I](fa: M[A])(f: A => M[B]): M[B] = Monad[M].flatMap(fa)(f)
  }
}
