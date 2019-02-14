package ru.buzden.timejur.value

import cats.Semigroup
import cats.syntax.semigroup._

final case class TimedValue[+A, +T](value: A, time: T)

object TimedValue extends TimedValueInstances

trait TimedValueInstances {
  implicit def tvSemigroup[A: Semigroup, T: Semigroup]: Semigroup[TimedValue[A, T]] = (tv1, tv2) =>
    TimedValue(tv1.value |+| tv2.value, tv1.time |+| tv2.time)
}
