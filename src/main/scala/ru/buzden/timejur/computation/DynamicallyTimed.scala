package ru.buzden.timejur.computation

import cats.arrow.ArrowChoice
import cats.{Contravariant, Functor, Monoid}

/**
  * Data structure representing a timed computation (i.e., a computation
  * that spends some model time to get its result) where
  * time is known only dynamically, i.e. only for each particular input
  * of the computation.
  *
  * @tparam A input type for the computation
  * @tparam B resulting type of the computation
  * @tparam T type for time
  * @param f function that defines computation **and** spent model time for each input
  */
final case class DynamicallyTimed[-A, +B, +T](f: A => (B, T))

object DynamicallyTimed {
  implicit def dtFunctor[X, T]: Functor[DynamicallyTimed[X, ?, T]] = new Functor[DynamicallyTimed[X, ?, T]] {
    override def map[A, B](fa: DynamicallyTimed[X, A, T])(f: A => B): DynamicallyTimed[X, B, T] =
      DynamicallyTimed { x =>
        val (intermediate, time) = fa.f(x)
        (f(intermediate), time)
      }
  }

  implicit def dtContravariant[Y, T]: Contravariant[DynamicallyTimed[?, Y, T]] = new Contravariant[DynamicallyTimed[?, Y, T]] {
    override def contramap[A, B](fa: DynamicallyTimed[A, Y, T])(f: B => A): DynamicallyTimed[B, Y, T] =
      DynamicallyTimed(fa.f `compose` f)
  }

  implicit def atArrowChoice[T: Monoid]: ArrowChoice[DynamicallyTimed[?, ?, T]] = new ArrowChoice[DynamicallyTimed[?, ?, T]] {
    /** Simple type alias for the sake of tacitness */
    type =?|>[A, B] = DynamicallyTimed[A, B, T]

    override def lift[A, B](f: A => B): A =?|> B = ???

    override def first[A, B, C](fa: A =?|> B): (A, C) =?|> (B, C) = ???

    override def compose[A, B, C](f: B =?|> C, g: A =?|> B): A =?|> C = ???

    override def choose[A, B, C, D](f: A =?|> C)(g: B =?|> D): Either[A, B] =?|> Either[C, D] = ???
  }
}
