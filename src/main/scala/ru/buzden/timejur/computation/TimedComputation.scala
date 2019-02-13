package ru.buzden.timejur.computation

import cats.arrow.{Arrow, ArrowChoice}
import cats.instances.function._
import cats.syntax.order._
import cats.syntax.semigroup._
import cats.{Contravariant, Functor, Monoid, Order}

case class TimedComputation[A, B, T](computation: A => B, time: T)

object TimedComputation {
  implicit def tcFunctor[X, T]: Functor[TimedComputation[X, ?, T]] = new Functor[TimedComputation[X, ?, T]] {
    override def map[A, B](fa: TimedComputation[X, A, T])(f: A => B): TimedComputation[X, B, T] =
      TimedComputation(fa.computation `andThen` f, fa.time)
  }

  implicit def tcContravariant[Y, T]: Contravariant[TimedComputation[?, Y, T]] = new Contravariant[TimedComputation[?, Y, T]] {
    override def contramap[A, B](fa: TimedComputation[A, Y, T])(f: B => A): TimedComputation[B, Y, T] =
      TimedComputation(fa.computation `compose` f, fa.time)
  }

  implicit def tcArrow[T: Monoid]: Arrow[TimedComputation[?, ?, T]] = new TCArrow[T]
}

private class TCArrow[T: Monoid] extends Arrow[TimedComputation[?, ?, T]] {
  /** Simple type alias for the sake of tacitness */
  type =|>[A, B] = TimedComputation[A, B, T]

  override def lift[A, B](f: A => B): A =|> B = TimedComputation(f, Monoid[T].empty)

  override def compose[A, B, C](f: B =|> C, g: A =|> B): A =|> C =
    TimedComputation(f.computation `compose` g.computation, f.time |+| g.time)

  override def first[A, B, C](fa: A =|> B): (A, C) =|> (B, C) =
    TimedComputation(Arrow[Function1].first(fa.computation), fa.time)
}

private class TCArrowChoice[T: Monoid:Order] extends TCArrow[T] with ArrowChoice[TimedComputation[?, ?, T]] {
  override def choose[A, B, C, D](f: A =|> C)(g: B =|> D): Either[A, B] =|> Either[C, D] =
    TimedComputation(ArrowChoice[Function1].choose(f.computation)(g.computation), f.time `max` g.time)
}
