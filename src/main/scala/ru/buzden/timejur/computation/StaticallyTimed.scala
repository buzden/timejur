package ru.buzden.timejur.computation

import cats.arrow.{Arrow, ArrowChoice}
import cats.instances.function._
import cats.syntax.arrowChoice._
import cats.syntax.order._
import cats.syntax.semigroup._
import cats.{Contravariant, Functor, Monoid, Order}

case class StaticallyTimed[-A, +B, +T](computation: A => B, time: T)

object StaticallyTimed {
  implicit def tcFunctor[X, T]: Functor[StaticallyTimed[X, ?, T]] = new Functor[StaticallyTimed[X, ?, T]] {
    override def map[A, B](fa: StaticallyTimed[X, A, T])(f: A => B): StaticallyTimed[X, B, T] =
      StaticallyTimed(fa.computation `andThen` f, fa.time)
  }

  implicit def tcContravariant[Y, T]: Contravariant[StaticallyTimed[?, Y, T]] = new Contravariant[StaticallyTimed[?, Y, T]] {
    override def contramap[A, B](fa: StaticallyTimed[A, Y, T])(f: B => A): StaticallyTimed[B, Y, T] =
      StaticallyTimed(fa.computation `compose` f, fa.time)
  }

  implicit def tcArrowChoice[T: Monoid:Order]: ArrowChoice[StaticallyTimed[?, ?, T]] = new TCArrowChoice[T]
  implicit def tcArrow[T: Monoid]: Arrow[StaticallyTimed[?, ?, T]] = new TCArrow[T]
}

private class TCArrow[T: Monoid] extends Arrow[StaticallyTimed[?, ?, T]] {
  /** Simple type alias for the sake of tacitness */
  type =|>[A, B] = StaticallyTimed[A, B, T]

  override def lift[A, B](f: A => B): A =|> B = StaticallyTimed(f, Monoid[T].empty)

  override def compose[A, B, C](f: B =|> C, g: A =|> B): A =|> C =
    StaticallyTimed(f.computation `compose` g.computation, f.time |+| g.time)

  override def first[A, B, C](fa: A =|> B): (A, C) =|> (B, C) =
    StaticallyTimed(Arrow[Function1].first(fa.computation), fa.time)
}

private class TCArrowChoice[T: Monoid:Order] extends TCArrow[T] with ArrowChoice[StaticallyTimed[?, ?, T]] {
  override def choose[A, B, C, D](f: A =|> C)(g: B =|> D): Either[A, B] =|> Either[C, D] =
    StaticallyTimed(f.computation +++ g.computation, f.time `max` g.time)
}
