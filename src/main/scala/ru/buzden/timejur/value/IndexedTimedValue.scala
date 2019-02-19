package ru.buzden.timejur.value

import ru.buzden.typelevel._

final case class IndexedTimedValue[A, T, ATime <: X[T]](value: A)

object IndexedTimedValue {
  implicit def indexedMonadForIndexedTimedValue[T, Z[_, _]](implicit im2: IndexingMonoid[T, Z]): IndexedMonad[T, IndexedTimedValue, Z] = new IndexedMonad[T, IndexedTimedValue, Z] {
    override val im: IndexingMonoid[T, Z] = im2
    import im._

    override def pure[A](a: A): IndexedTimedValue[A, T, Empty] =
      IndexedTimedValue[A, T, Empty](a)

    override def flatMap[A, ATime <: X[T], B, BTime <: X[T]]
        (fa: IndexedTimedValue[A, T, ATime])(f: A => IndexedTimedValue[B, T, BTime]): IndexedTimedValue[B, T, ATime |+| BTime] =
      IndexedTimedValue[B, T, ATime |+| BTime](f(fa.value).value)
  }
}
