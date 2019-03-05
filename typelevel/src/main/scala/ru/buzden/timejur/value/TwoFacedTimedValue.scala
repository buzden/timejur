package ru.buzden.timejur.value

import cats.Functor
import cats.syntax.functor._
import ru.buzden.iz._

/** Timed value that contains time both at the type and value level */
final case class TwoFacedTimedValue[A, ATime](value: A, time: ATime)

object TwoFacedTimedValue {
  implicit def izMonadForTwoFacedTimedValue[T: TwoFacedMonoid]: IzMonad[T, TwoFacedTimedValue] = new IzMonad[T, TwoFacedTimedValue] {
    override val im: TwoFacedMonoid[T] = implicitly
    import im._

    override type PureR[A] = Functor[EmptyR] IFT EmptyR[A]
    override type FlatMapR[ATime, BTime, C] = Functor[CombineR[ATime, BTime, ?]] IFT CombineR[ATime, BTime, C]

    override def pure[A](a: A): Functor[EmptyR] IFT EmptyR[TwoFacedTimedValue[A, Empty]] = IFT { implicit fe =>
      empty `map` { e => TwoFacedTimedValue[A, Empty](a, e) }
    }

    override def flatMap[A, ATime <: T, B, BTime <: T](fa: TwoFacedTimedValue[A, ATime])(f: A => TwoFacedTimedValue[B, BTime]): Functor[CombineR[ATime, BTime, ?]] IFT CombineR[ATime, BTime, TwoFacedTimedValue[B, ATime |+| BTime]] =
      IFT { implicit fc =>
        val fb = f(fa.value)
        im.combine[ATime, BTime](fa.time, fb.time) `map` { s => TwoFacedTimedValue[B, ATime |+| BTime](fb.value, s) }
      }
  }
}
