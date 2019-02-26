package ru.buzden.timejur.value

import ru.buzden.typelevel._

/** Timed value that contains time only at the type level */
final case class IndexedTimedValue[A, ATime](value: A)

object IndexedTimedValue {
  implicit def indexedMonadForIndexedTimedValue[T: IndexingMonoid]: IndexedMonad[T, IndexedTimedValue] = new IndexedMonad[T, IndexedTimedValue] {
    override val im: IndexingMonoid[T] = implicitly
    import im._

    override type PureR[A] = A
    override type FlatMapR[ATime, BTime, C] = C

    override def pure[A](a: A): IndexedTimedValue[A, Empty] =
      IndexedTimedValue[A, Empty](a)

    override def flatMap[A, ATime <: T, B, BTime <: T]
        (fa: IndexedTimedValue[A, ATime])(f: A => IndexedTimedValue[B, BTime]): IndexedTimedValue[B, ATime |+| BTime] =
      IndexedTimedValue[B, ATime |+| BTime](f(fa.value).value)
  }
}
