package ru.buzden.timejur.value

import cats.instances.int._
import cats.instances.long._
import cats.instances.string._
import cats.instances.tuple._
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.{FunctorTests, MonadTests}
import cats.syntax.apply._
import cats.{Eq, Semigroup}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.cats.implicits._
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.timejur.Time

//noinspection TypeAnnotation
object TimedValueSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  ${semigroupLaws[Int, Time]}
  ${functorLaws[Time]}
  $monadLaws
  """

  def semigroupLaws[A: Semigroup:Eq:Arbitrary, T: Semigroup:Eq:Arbitrary] = checkAll("simple timed value",
    SemigroupTests[TimedValue[A, T]].semigroup
  )

  def functorLaws[T: Eq:Arbitrary] = checkAll("simple timed value",
    FunctorTests[TimedValue[?, T]].functor[Int, String, Long]
  )

  type TV[A] = TimedValue[A, Time] // This type is a workaround of a compiler bug.
  def monadLaws(implicit wtf: Isomorphisms[TV]) = checkAll("simple timed value",
    MonadTests[TV].monad[Int, String, Long]
  )

  implicit def arbStaticallyTimed[A: Arbitrary, T: Arbitrary]: Arbitrary[TimedValue[A, T]] =
    Arbitrary { (arbitrary[A], arbitrary[T]) `mapN` { TimedValue(_, _) } }
}
