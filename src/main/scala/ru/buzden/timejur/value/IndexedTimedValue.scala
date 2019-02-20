package ru.buzden.timejur.value

import ru.buzden.typelevel._

final case class IndexedTimedValue[A, ATime](value: A)

object IndexedTimedValue {
  implicit def indexedMonadForIndexedTimedValue[T: IndexingMonoidZZ]: IndexedMonad[T, IndexedTimedValue] = new IndexedMonad[T, IndexedTimedValue] {
    override val im: IndexingMonoidZZ[T] = implicitly
    import im._

    override def pure[A](a: A): IndexedTimedValue[A, Empty] =
      IndexedTimedValue[A, Empty](a)

    override def flatMap[A, ATime <: T, B, BTime <: T]
        (fa: IndexedTimedValue[A, ATime])(f: A => IndexedTimedValue[B, BTime]): IndexedTimedValue[B, ATime |+| BTime] =
      IndexedTimedValue[B, ATime |+| BTime](f(fa.value).value)
  }
}
