package ru.buzden.timejur.computation

import cats.instances.either._
import cats.instances.int._
import cats.instances.long._
import cats.instances.tuple._
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.syntax.apply._
import cats.{Eq, Monoid, Order}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

//noinspection TypeAnnotation
object StaticallyTimedSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  ${covariantFunctorLaws[Int, Time]}
  ${contravariantFunctorLaws[Int, Time]}
  ${arrowLaws[Time]}
  ${arrowChoiceLaws[Time]}
  """

  def covariantFunctorLaws[A: Arbitrary:Cogen, T: Arbitrary:Eq] = checkAll("statically timed computation",
    FunctorTests[StaticallyTimed[A, ?, T]].functor[Int, String, Long]
  )

  def contravariantFunctorLaws[B: Arbitrary:Eq, T: Arbitrary:Eq] = checkAll("statically timed computation",
    ContravariantTests[StaticallyTimed[?, B, T]].contravariant[Int, String, Long]
  )

  def arrowLaws[T: Arbitrary:Eq:Monoid] = checkAll("statically timed computation",
    ArrowTests[StaticallyTimed[?, ?, T]].arrow[Int, Long, Int, Long, Int, Long]
  )

  def arrowChoiceLaws[T: Arbitrary:Order:Monoid] = checkAll("statically timed computation",
    ArrowChoiceTests[StaticallyTimed[?, ?, T]].arrowChoice[Int, Int, Long, Int, Int, Long]
  )

  implicit def arbStaticallyTimed[A: Cogen, B: Arbitrary, T: Arbitrary]: Arbitrary[StaticallyTimed[A, B, T]] =
    Arbitrary { (arbitrary[A => B], arbitrary[T]) `mapN` { StaticallyTimed(_, _) } }

  implicit def eqStaticallyTimed[A: Arbitrary, B: Eq, T: Eq]: Eq[StaticallyTimed[A, B, T]] =
    Eq.and(Eq.by(_.f), Eq.by(_.time))
}

/** Simple time for testing purposes representing a non-negative integer */
final case class Time(v: Int) extends AnyVal
object Time {
  implicit val orderTime: Order[Time] = Order.by(_.v)
  implicit val monoidTime: Monoid[Time] = new Monoid[Time] {
    override def empty: Time = Time(0)
    override def combine(x: Time, y: Time): Time = Time(x.v + y.v)
  }

  def nonNegNum[N: Numeric:Choose]: Gen[N] =
    frequency(1 -> const(implicitly[Numeric[N]].zero), 99 -> posNum[N])
  implicit val arbTime: Arbitrary[Time] = Arbitrary(nonNegNum[Int] `map` { Time(_) } )
}
