package ru.buzden.timejur.computation

import cats.{Contravariant, Functor}

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
}
