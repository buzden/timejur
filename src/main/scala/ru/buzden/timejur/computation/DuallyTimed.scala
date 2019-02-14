package ru.buzden.timejur.computation

import cats.arrow.ArrowChoice
import cats.{Contravariant, Functor, Monoid, Order}
import cats.syntax.order._
import cats.syntax.semigroup._

/**
  * Data structure representing a timed computation (i.e., a computation
  * that spends some model time to get its result) that contains both
  * dynamic time component (depending on the input) and
  * static time component (maximum time that can be spent).
  *
  * Static time component is stored in a value.
  * That is, the following invariant must always hold:
  * `forall a: A :: f(a)._2 <= maxTime`
  *
  * @tparam A       input type for the computation
  * @tparam B       resulting type of the computation
  * @tparam T       type for time
  * @param  f       function that defines computation **and** spent model time for each input
  * @param  maxTime maximum spent time for all possible computation inputs
  */
final class DuallyTimed[-A, +B, +T] private[DuallyTimed] (val f: A => (B, T), val maxTime: T)
// todo to make `DuallyTimed` case class again as soon as compiler would generate
//  unambiguous `apply` for case classes with private constructor.

object DuallyTimed extends DuallyTimedInstances {
  /** Creates a dually timed computation with time bounded by the given `maxTime` value */
  def apply[A, B, T: Order](rawF: A => (B, T), maxTime: T): DuallyTimed[A, B, T] = create(maxTime)(rawF)

  /** Creates a dually timed computation with time bounded by the given `maxTime` value */
  def create[A, B, T: Order](maxTime: T)(rawF: A => (B, T)): DuallyTimed[A, B, T] =
    new DuallyTimed[A, B, T](rawF `andThen` { case (b, t) => (b, t `min` maxTime) }, maxTime)

  def unapply[A, B, T](dut: DuallyTimed[A, B, T]): Option[(A => (B, T), T)] = Some((dut.f, dut.maxTime))
}

trait DuallyTimedInstances {
  implicit def dutFunctor[X, T: Order]: Functor[DuallyTimed[X, ?, T]] = new Functor[DuallyTimed[X, ?, T]] {
    override def map[A, B](fa: DuallyTimed[X, A, T])(f: A => B): DuallyTimed[X, B, T] =
      DuallyTimed.create(fa.maxTime){ x =>
        val (intermediate, time) = fa.f(x)
        (f(intermediate), time)
      }
  }

  implicit def dutContravariant[Y, T: Order]: Contravariant[DuallyTimed[?, Y, T]] = new Contravariant[DuallyTimed[?, Y, T]] {
    override def contramap[A, B](fa: DuallyTimed[A, Y, T])(f: B => A): DuallyTimed[B, Y, T] =
      DuallyTimed.create(fa.maxTime)(fa.f `compose` f)
  }

  implicit def dutArrowChoice[T: Monoid:Order]: ArrowChoice[DuallyTimed[?, ?, T]] = new ArrowChoice[DuallyTimed[?, ?, T]] {
    /** Simple type alias for the sake of tacitness */
    type ==|>[A, B] = DuallyTimed[A, B, T]

    override def lift[A, B](f: A => B): A ==|> B =
      DuallyTimed.create(Monoid.empty)(f `andThen` { (_, Monoid.empty) })

    override def first[A, B, C](fa: A ==|> B): (A, C) ==|> (B, C) = DuallyTimed.create(fa.maxTime) { case (a, c) =>
      val (b, time) = fa.f(a)
      ((b, c), time)
    }

    override def compose[A, B, C](f: B ==|> C, g: A ==|> B): A ==|> C =
      DuallyTimed.create(f.maxTime |+| g.maxTime) { a =>
        val (b, timeAB) = g.f(a)
        val (c, timeBC) = f.f(b)
        (c, timeAB |+| timeBC)
      }

    override def choose[A, B, C, D](f: A ==|> C)(g: B ==|> D): Either[A, B] ==|> Either[C, D] =
      DuallyTimed.create(f.maxTime `max` g.maxTime) {
        case Left(a) =>
          val (c, timeAC) = f.f(a)
          (Left(c), timeAC)
        case Right(b) =>
          val (d, timeBD) = g.f(b)
          (Right(d), timeBD)
      }
  }
}
