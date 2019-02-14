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
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen}
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.timejur.Time

//noinspection TypeAnnotation
object DuallyTimedSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  ${covariantFunctorLaws[Int, Time]}
  ${contravariantFunctorLaws[Int, Time]}
  ${arrowChoiceLaws[Time]}
  """

  def covariantFunctorLaws[A: Arbitrary:Cogen, T: Arbitrary:Order] = checkAll("dually timed computation",
    FunctorTests[DuallyTimed[A, ?, T]].functor[Int, String, Long]
  )

  def contravariantFunctorLaws[B: Arbitrary:Eq, T: Arbitrary:Order] = checkAll("dually timed computation",
    ContravariantTests[DuallyTimed[?, B, T]].contravariant[Int, String, Long]
  )

  def arrowChoiceLaws[T: Arbitrary:Order:Monoid] = checkAll("dually timed computation",
    ArrowChoiceTests[DuallyTimed[?, ?, T]].arrowChoice[Int, Int, Long, Int, Int, Long]
  )

  implicit def arbDuallyTimed[A: Cogen, B: Arbitrary, T: Arbitrary:Order]: Arbitrary[DuallyTimed[A, B, T]] =
    Arbitrary { (arbitrary[A => (B, T)], arbitrary[T]) `mapN` { DuallyTimed(_, _) } }

  implicit def eqDuallyTimed[A: Arbitrary, B: Eq, T: Eq]: Eq[DuallyTimed[A, B, T]] =
    Eq.and(Eq.by(_.f), Eq.by(_.maxTime))
}
