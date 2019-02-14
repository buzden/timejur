package ru.buzden.timejur.computation

import cats.arrow.{Arrow, ArrowChoice}
import cats.instances.function._
import cats.syntax.arrowChoice._
import cats.syntax.order._
import cats.syntax.semigroup._
import cats.{Contravariant, Functor, Monoid, Order}

/**
  * Data structure representing a timed computation (i.e., a computation
  * that spends some model time to get its result) where
  * time is known statically, without running the computation.
  *
  * @tparam A input type for the computation
  * @tparam B resulting type of the computation
  * @tparam T type for time
  * @param f function for the computation
  * @param time model time spent by the computation
  */
final case class StaticallyTimed[-A, +B, +T](f: A => B, time: T)

object StaticallyTimed {
  implicit def tcFunctor[X, T]: Functor[StaticallyTimed[X, ?, T]] = new Functor[StaticallyTimed[X, ?, T]] {
    override def map[A, B](fa: StaticallyTimed[X, A, T])(f: A => B): StaticallyTimed[X, B, T] =
      StaticallyTimed(fa.f `andThen` f, fa.time)
  }

  implicit def tcContravariant[Y, T]: Contravariant[StaticallyTimed[?, Y, T]] = new Contravariant[StaticallyTimed[?, Y, T]] {
    override def contramap[A, B](fa: StaticallyTimed[A, Y, T])(f: B => A): StaticallyTimed[B, Y, T] =
      StaticallyTimed(fa.f `compose` f, fa.time)
  }

  implicit def tcArrowChoice[T: Monoid:Order]: ArrowChoice[StaticallyTimed[?, ?, T]] = new TCArrowChoice[T]
  implicit def tcArrow[T: Monoid]: Arrow[StaticallyTimed[?, ?, T]] = new TCArrow[T]
}

private class TCArrow[T: Monoid] extends Arrow[StaticallyTimed[?, ?, T]] {
  /** Simple type alias for the sake of tacitness */
  type =|>[A, B] = StaticallyTimed[A, B, T]

  override def lift[A, B](f: A => B): A =|> B = StaticallyTimed(f, Monoid[T].empty)

  override def compose[A, B, C](f: B =|> C, g: A =|> B): A =|> C =
    StaticallyTimed(f.f `compose` g.f, f.time |+| g.time)

  override def first[A, B, C](fa: A =|> B): (A, C) =|> (B, C) =
    StaticallyTimed(Arrow[Function1].first(fa.f), fa.time)
}

private class TCArrowChoice[T: Monoid:Order] extends TCArrow[T] with ArrowChoice[StaticallyTimed[?, ?, T]] {
  override def choose[A, B, C, D](f: A =|> C)(g: B =|> D): Either[A, B] =|> Either[C, D] =
    StaticallyTimed(f.f +++ g.f, f.time `max` g.time)
}
