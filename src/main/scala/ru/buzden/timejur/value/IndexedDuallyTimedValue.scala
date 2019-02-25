package ru.buzden.timejur.value

import cats.Functor
import ru.buzden.typelevel._
import cats.syntax.functor._

final case class IndexedDuallyTimedValue[A, ATime](value: A, time: ATime)

object IndexedDuallyTimedValue {
  implicit def indexedMonadForIndexedTimedValue[T: EmergingIndexingMonoid]: IndexedMonad[T, IndexedDuallyTimedValue] = new IndexedMonad[T, IndexedDuallyTimedValue] {
    override val im: EmergingIndexingMonoid[T] = implicitly
    import im._

    override type PureR[A] = Functor[EmptyR] IFT EmptyR[A]
    override type FlatMapR[ATime, BTime, C] = Functor[CombinationR[ATime, BTime, ?]] IFT CombinationR[ATime, BTime, C]

    override def pure[A](a: A): Functor[EmptyR] IFT EmptyR[IndexedDuallyTimedValue[A, Empty]] = IFT { implicit fe =>
      empty `map` { e => IndexedDuallyTimedValue[A, Empty](a, e) }
    }

    override def flatMap[A, ATime <: T, B, BTime <: T]
        (fa: IndexedDuallyTimedValue[A, ATime])(f: A => IndexedDuallyTimedValue[B, BTime]): Functor[CombinationR[ATime, BTime, ?]] IFT CombinationR[ATime, BTime, IndexedDuallyTimedValue[B, ATime |+| BTime]] = IFT { implicit fc =>
      im.combine[ATime, BTime] `map` { s => IndexedDuallyTimedValue[B, ATime |+| BTime](f(fa.value).value, s) }
    }
  }
}
