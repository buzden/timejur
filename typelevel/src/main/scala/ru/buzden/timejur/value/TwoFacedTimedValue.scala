package ru.buzden.timejur.value

import ru.buzden.iz._

/** Timed value that contains time both at the type and value level */
final case class TwoFacedTimedValue[A, ATime](value: A, time: ATime)

object TwoFacedTimedValue {
  implicit def izMonadForTwoFacedTimedValue[T: TwoFacedMonoid]: IzMonad[T, TwoFacedTimedValue] = new IzMonad[T, TwoFacedTimedValue] {
    override val im: TwoFacedMonoid[T] = implicitly
    import im._

    override def pure[A](a: A): TwoFacedTimedValue[A, Empty] = TwoFacedTimedValue[A, Empty](a, empty)

    override def flatMap[A, ATime <: T, B, BTime <: T](fa: TwoFacedTimedValue[A, ATime])(f: A => TwoFacedTimedValue[B, BTime]): TwoFacedTimedValue[B, ATime |+| BTime] = {
      val fb = f(fa.value)
      TwoFacedTimedValue[B, ATime |+| BTime](fb.value, im.combine(fa.time, fb.time))
    }
  }
}
