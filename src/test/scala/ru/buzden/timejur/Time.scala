package ru.buzden.timejur

import cats.instances.int._
import cats.{Monoid, Order}
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

/** Simple time for testing purposes representing (mostly) a non-negative integer */
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