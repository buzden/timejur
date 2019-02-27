package ru.buzden.timejur.value

import cats.Functor
import cats.syntax.functor._
import ru.buzden.iz._

/** Timed value that contains time both at the type and value level */
final case class IndexedEmergedTimedValue[A, ATime](value: A, time: ATime)

object IndexedEmergedTimedValue {
  implicit def indexedMonadForIndexedTimedValue[T: EmergingIndexingMonoid]: IndexedMonad[T, IndexedEmergedTimedValue] = new IndexedMonad[T, IndexedEmergedTimedValue] {
    override val im: EmergingIndexingMonoid[T] = implicitly
    import im._

    override type PureR[A] = Functor[EmptyR] IFT EmptyR[A]
    override type FlatMapR[ATime, BTime, C] = Functor[CombinationR[ATime, BTime, ?]] IFT CombinationR[ATime, BTime, C]

    override def pure[A](a: A): Functor[EmptyR] IFT EmptyR[IndexedEmergedTimedValue[A, Empty]] = IFT { implicit fe =>
      empty `map` { e => IndexedEmergedTimedValue[A, Empty](a, e) }
    }

    override def flatMap[A, ATime <: T, B, BTime <: T]
    (fa: IndexedEmergedTimedValue[A, ATime])(f: A => IndexedEmergedTimedValue[B, BTime]): Functor[CombinationR[ATime, BTime, ?]] IFT CombinationR[ATime, BTime, IndexedEmergedTimedValue[B, ATime |+| BTime]] = IFT { implicit fc =>
      im.combine[ATime, BTime] `map` { s => IndexedEmergedTimedValue[B, ATime |+| BTime](f(fa.value).value, s) }
    }
  }
}
