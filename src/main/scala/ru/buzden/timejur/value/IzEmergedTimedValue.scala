package ru.buzden.timejur.value

import cats.Functor
import cats.syntax.functor._
import ru.buzden.iz._

/** Timed value that contains time both at the type and value level */
final case class IzEmergedTimedValue[A, ATime](value: A, time: ATime)

object IzEmergedTimedValue {
  implicit def indexedMonadForIndexedTimedValue[T: EmergingTLMonoid]: IzMonad[T, IzEmergedTimedValue] = new IzMonad[T, IzEmergedTimedValue] {
    override val im: EmergingTLMonoid[T] = implicitly
    import im._

    override type PureR[A] = Functor[EmptyR] IFT EmptyR[A]
    override type FlatMapR[ATime, BTime, C] = Functor[CombinationR[ATime, BTime, ?]] IFT CombinationR[ATime, BTime, C]

    override def pure[A](a: A): Functor[EmptyR] IFT EmptyR[IzEmergedTimedValue[A, Empty]] = IFT { implicit fe =>
      empty `map` { e => IzEmergedTimedValue[A, Empty](a, e) }
    }

    override def flatMap[A, ATime <: T, B, BTime <: T]
    (fa: IzEmergedTimedValue[A, ATime])(f: A => IzEmergedTimedValue[B, BTime]): Functor[CombinationR[ATime, BTime, ?]] IFT CombinationR[ATime, BTime, IzEmergedTimedValue[B, ATime |+| BTime]] = IFT { implicit fc =>
      im.combine[ATime, BTime] `map` { s => IzEmergedTimedValue[B, ATime |+| BTime](f(fa.value).value, s) }
    }
  }
}
