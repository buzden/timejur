package ru.buzden.timejur.value

import cats.syntax.semigroup._
import cats.{Functor, Semigroup}

final case class TimedValue[+A, +T](value: A, time: T)

object TimedValue extends TimedValueInstances

trait TimedValueInstances {
  implicit def tvSemigroup[A: Semigroup, T: Semigroup]: Semigroup[TimedValue[A, T]] = (tv1, tv2) =>
    TimedValue(tv1.value |+| tv2.value, tv1.time |+| tv2.time)

  implicit def tvFunctor[T]: Functor[TimedValue[?, T]] = new Functor[TimedValue[?, T]] {
    override def map[A, B](fa: TimedValue[A, T])(f: A => B): TimedValue[B, T] =
      TimedValue(f(fa.value), fa.time)
  }
}
