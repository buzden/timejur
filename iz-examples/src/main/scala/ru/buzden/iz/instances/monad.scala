package ru.buzden.iz.instances

import cats.Monad
import ru.buzden.iz._

object monad {
  type I2A[M[_], A, I_A] = M[A]
  implicit def monadIsIzMonad[I: TypeLevelMonoid, M[_]: Monad]: IzMonad[I, I2A[M, ?, ?]] = new IzMonad[I, I2A[M, ?, ?]] {
    override val im: TypeLevelMonoid[I] = implicitly

    override type PureR[A] = A
    override type FlatMapR[I_A, I_B, C] = C

    override def pure[A](a: A): M[A] = Monad[M].pure(a)
    override def flatMap[A, I_A <: I, B, I_B <: I](fa: M[A])(f: A => M[B]): M[B] = Monad[M].flatMap(fa)(f)
  }
}