package ru.buzden.timejur.computation

import cats.arrow.ArrowChoice
import cats.syntax.semigroup._
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

object DynamicallyTimed extends DynamicallyTimedInstances

trait DynamicallyTimedInstances {
  implicit def dytFunctor[X, T]: Functor[DynamicallyTimed[X, ?, T]] = new Functor[DynamicallyTimed[X, ?, T]] {
    override def map[A, B](fa: DynamicallyTimed[X, A, T])(f: A => B): DynamicallyTimed[X, B, T] =
      DynamicallyTimed { x =>
        val (intermediate, time) = fa.f(x)
        (f(intermediate), time)
      }
  }

  implicit def dytContravariant[Y, T]: Contravariant[DynamicallyTimed[?, Y, T]] = new Contravariant[DynamicallyTimed[?, Y, T]] {
    override def contramap[A, B](fa: DynamicallyTimed[A, Y, T])(f: B => A): DynamicallyTimed[B, Y, T] =
      DynamicallyTimed(fa.f `compose` f)
  }

  implicit def dytArrowChoice[T: Monoid]: ArrowChoice[DynamicallyTimed[?, ?, T]] = new ArrowChoice[DynamicallyTimed[?, ?, T]] {
    /** Simple type alias for the sake of tacitness */
    type =?|>[A, B] = DynamicallyTimed[A, B, T]

    override def lift[A, B](f: A => B): A =?|> B = DynamicallyTimed(f `andThen` { (_, Monoid.empty) })

    override def first[A, B, C](fa: A =?|> B): (A, C) =?|> (B, C) = DynamicallyTimed { case (a, c) =>
      val (b, time) = fa.f(a)
      ((b, c), time)
    }

    override def compose[A, B, C](f: B =?|> C, g: A =?|> B): A =?|> C = DynamicallyTimed { a =>
      val (b, timeAB) = g.f(a)
      val (c, timeBC) = f.f(b)
      (c, timeAB |+| timeBC)
    }

    override def choose[A, B, C, D](f: A =?|> C)(g: B =?|> D): Either[A, B] =?|> Either[C, D] =
      DynamicallyTimed {
        case Left(a) =>
          val (c, timeAC) = f.f(a)
          (Left(c), timeAC)
        case Right(b) =>
          val (d, timeBD) = g.f(b)
          (Right(d), timeBD)
      }
  }
}
