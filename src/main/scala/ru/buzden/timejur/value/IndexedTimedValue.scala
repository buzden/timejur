package ru.buzden.timejur.value

import ru.buzden.typelevel._

final case class IndexedTimedValue[A, T, ATime <: T with Singleton](value: A)

object IndexedTimedValue {
  implicit val indexedMonadForIndexedTimedValue: IndexedMonad[IndexedTimedValue] = new IndexedMonad[IndexedTimedValue] {
    override def pure[T, A](a: A)(implicit im: IndexingMonoid[T]): IndexedTimedValue[A, T, im.Empty] =
      IndexedTimedValue[A, T, im.Empty](a)

    override def flatMap[T, A, ATime <: T with Singleton, B, BTime <: T with Singleton]
        (fa: IndexedTimedValue[A, T, ATime])(f: A => IndexedTimedValue[B, T, BTime])
        (implicit is: IndexingSemigroup[T]): IndexedTimedValue[B, T, is.|+|[ATime, BTime]] =
      IndexedTimedValue[B, T, is.|+|[ATime, BTime]](f(fa.value).value)
  }
}
