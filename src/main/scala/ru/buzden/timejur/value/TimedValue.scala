package ru.buzden.timejur.value

import cats.syntax.semigroup._
import cats.{Functor, Monad, Monoid, Semigroup}

final case class TimedValue[+A, +T](value: A, time: T)

object TimedValue extends TimedValueInstances

trait TimedValueInstances {
  implicit def tvSemigroup[A: Semigroup, T: Semigroup]: Semigroup[TimedValue[A, T]] = (tv1, tv2) =>
    TimedValue(tv1.value |+| tv2.value, tv1.time |+| tv2.time)

  implicit def tvFunctor[T]: Functor[TimedValue[?, T]] = new Functor[TimedValue[?, T]] {
    override def map[A, B](fa: TimedValue[A, T])(f: A => B): TimedValue[B, T] =
      TimedValue(f(fa.value), fa.time)
  }

  implicit def tvMonad[T: Monoid]: Monad[TimedValue[?, T]] = new Monad[TimedValue[?, T]] {
    override def pure[A](x: A): TimedValue[A, T] = TimedValue(x, Monoid.empty)

    override def map[A, B](fa: TimedValue[A, T])(f: A => B): TimedValue[B, T] = tvFunctor[T].map(fa)(f)

    override def flatMap[A, B](fa: TimedValue[A, T])(f: A => TimedValue[B, T]): TimedValue[B, T] = {
      val TimedValue(b, timeB) = f(fa.value)
      TimedValue(b, fa.time |+| timeB)
    }

    override def tailRecM[A, B](a: A)(f: A => TimedValue[Either[A, B], T]): TimedValue[B, T] = {
      def go(currAB: Either[A, B], currSum: T): TimedValue[B, T] = currAB match {
        case Left(currA)  =>
          val TimedValue(nextAB, currT) = f(currA)
          go(nextAB, currSum |+| currT)
        case Right(b) => TimedValue(b, currSum)
      }

      val TimedValue(ab, t) = f(a)
      go(ab, t)
    }
  }
}
