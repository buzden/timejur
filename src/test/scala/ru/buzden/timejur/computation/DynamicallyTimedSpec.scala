package ru.buzden.timejur.computation

import cats.instances.either._
import cats.instances.int._
import cats.instances.long._
import cats.instances.tuple._
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.{Eq, Monoid, Order}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen}
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.timejur.Time

//noinspection TypeAnnotation
object DynamicallyTimedSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  ${covariantFunctorLaws[Int, Time]}
  ${contravariantFunctorLaws[Int, Time]}
  ${arrowChoiceLaws[Time]}
  """

  def covariantFunctorLaws[A: Arbitrary:Cogen, T: Arbitrary:Eq] = checkAll("dynamically timed computation",
    FunctorTests[DynamicallyTimed[A, ?, T]].functor[Int, String, Long]
  )

  def contravariantFunctorLaws[B: Arbitrary:Eq, T: Arbitrary:Eq] = checkAll("dynamically timed computation",
    ContravariantTests[DynamicallyTimed[?, B, T]].contravariant[Int, String, Long]
  )

  def arrowChoiceLaws[T: Arbitrary:Order:Monoid] = checkAll("dynamically timed computation",
    ArrowChoiceTests[DynamicallyTimed[?, ?, T]].arrowChoice[Int, Int, Long, Int, Int, Long]
  )

  implicit def arbDynamicallyTimed[A: Cogen, B: Arbitrary, T: Arbitrary]: Arbitrary[DynamicallyTimed[A, B, T]] =
    Arbitrary { arbitrary[A => (B, T)] `map` { DynamicallyTimed(_) } }

  implicit def eqDynamicallyTimed[A: Arbitrary, B: Eq, T: Eq]: Eq[DynamicallyTimed[A, B, T]] = Eq.by(_.f)
}
