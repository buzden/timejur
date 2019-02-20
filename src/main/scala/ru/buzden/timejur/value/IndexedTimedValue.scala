package ru.buzden.timejur.value

import ru.buzden.typelevel._

final case class IndexedTimedValue[A, T, ATime <: T](value: A)

object IndexedTimedValue {
  implicit def indexedMonadForIndexedTimedValue[T: IndexingMonoidZZ]: IndexedMonad[T, IndexedTimedValue[?, T, ?]] = new IndexedMonad[T, IndexedTimedValue[?, T, ?]] {
    override val im: IndexingMonoidZZ[T] = implicitly
    import im._

    override def pure[A](a: A): IndexedTimedValue[A, T, Empty] =
      IndexedTimedValue[A, T, Empty](a)

    override def flatMap[A, ATime <: T, B, BTime <: T]
        (fa: IndexedTimedValue[A, T, ATime])(f: A => IndexedTimedValue[B, T, BTime]): IndexedTimedValue[B, T, ATime |+| BTime] =
      IndexedTimedValue[B, T, ATime |+| BTime](f(fa.value).value)
  }
}
